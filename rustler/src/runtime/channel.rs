use crate::types::LocalPid;
use crate::{Decoder, Encoder, Env, Error, NifResult, OwnedEnv, ResourceArc, Term};
use futures_core::Stream;
use std::marker::PhantomData;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use tokio::sync::mpsc;

// Type-erased sender function for channel messages.
type SendFn = Arc<dyn Fn(Env, Term) -> NifResult<()> + Send + Sync + std::panic::RefUnwindSafe>;

/// Internal sender resource (type-erased for resource registration).
///
/// This is the actual resource registered with BEAM. It holds a type-erased
/// sender function that decodes Terms and sends them to the typed channel.
pub struct ChannelSenderInner {
    send_fn: SendFn,
}

impl crate::Resource for ChannelSenderInner {}

// Auto-register ChannelSenderInner resource
crate::codegen_runtime::inventory::submit! {
    crate::resource::Registration::new::<ChannelSenderInner>()
}

/// Type-safe wrapper around the channel sender resource.
///
/// This is returned to Elixir and can be used to send messages of type `Request`
/// to the running task. It also serves as the task reference for pattern matching
/// on response messages.
pub struct ChannelSender<Request> {
    inner: ResourceArc<ChannelSenderInner>,
    _phantom: PhantomData<Request>,
}

impl<Request> Clone for ChannelSender<Request> {
    fn clone(&self) -> Self {
        ChannelSender {
            inner: self.inner.clone(),
            _phantom: PhantomData,
        }
    }
}

unsafe impl<Request: Send> Send for ChannelSender<Request> {}
unsafe impl<Request: Sync> Sync for ChannelSender<Request> {}

/// Cloneable sender for responses that can be passed to spawned tasks.
///
/// This allows spawned subtasks to send their own responses back to Elixir,
/// all tagged with the same channel sender reference.
pub struct ResponseSender<Request, Response> {
    sender: ChannelSender<Request>,
    pid: LocalPid,
    _phantom: PhantomData<Response>,
}

impl<Request, Response> Clone for ResponseSender<Request, Response> {
    fn clone(&self) -> Self {
        ResponseSender {
            sender: self.sender.clone(),
            pid: self.pid,
            _phantom: PhantomData,
        }
    }
}

unsafe impl<Request: Send, Response: Send> Send for ResponseSender<Request, Response> {}
unsafe impl<Request: Sync, Response: Sync> Sync for ResponseSender<Request, Response> {}

impl<Request, Response> ResponseSender<Request, Response>
where
    Response: Encoder + Send + 'static,
{
    /// Send a response message to the calling process.
    ///
    /// The message will be sent as `{channel_sender, response}`.
    pub fn send(&self, response: Response) {
        let mut env = OwnedEnv::new();
        let sender = self.sender.clone();
        let _ = env.send_and_clear(&self.pid, move |env| (sender, response).encode(env));
    }
}

impl<Request> Encoder for ChannelSender<Request> {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.inner.encode(env)
    }
}

impl<'a, Request: 'a> Decoder<'a> for ChannelSender<Request> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let inner: ResourceArc<ChannelSenderInner> = term.decode()?;
        Ok(ChannelSender {
            inner,
            _phantom: PhantomData,
        })
    }
}

/// Bidirectional channel for typed communication with async tasks.
///
/// `Channel<Request, Response>` provides both:
/// - **Receiving requests** from the calling Elixir process via `Stream` trait or `recv()`
/// - **Sending responses** back to the caller via `send()`
///
/// The channel implements `Stream`, allowing idiomatic async iteration over incoming requests.
/// All response messages are automatically tagged with the channel sender reference.
///
/// # Type Parameters
///
/// - `Request`: Type of messages the task receives from Elixir (default: `()` for one-way tasks)
/// - `Response`: Type of messages the task sends back to Elixir
///
/// # Examples
///
/// ## One-way task (no incoming messages)
///
/// ```ignore
/// #[rustler::task]
/// async fn compute(channel: Channel<(), i64>, n: i64) -> i64 {
///     channel.send(n / 2); // Send progress
///     tokio::time::sleep(Duration::from_millis(100)).await;
///     n * 2 // Final result
/// }
/// ```
///
/// ## Interactive task with Stream trait
///
/// ```ignore
/// use futures::StreamExt; // for next()
///
/// #[rustler::task]
/// async fn interactive(channel: Channel<Command, String>) -> String {
///     let mut count = 0;
///
///     // Stream trait - idiomatic async iteration
///     while let Some(cmd) = channel.next().await {
///         match cmd {
///             Command::Stop => break,
///             Command::Process(x) => {
///                 count += 1;
///                 channel.send(format!("Processed: {}", x));
///             }
///         }
///     }
///
///     format!("Processed {} commands", count)
/// }
/// ```
///
/// ## Using recv() directly
///
/// ```ignore
/// #[rustler::task]
/// async fn simple(channel: Channel<bool, String>) -> String {
///     if let Some(should_proceed) = channel.recv().await {
///         if should_proceed {
///             channel.send("Processing...".to_string());
///             // do work
///             return "Done".to_string();
///         }
///     }
///     "Cancelled".to_string()
/// }
/// ```
pub struct Channel<Request = (), Response = ()> {
    receiver: mpsc::UnboundedReceiver<Request>,
    sender: ChannelSender<Request>,
    pid: LocalPid,
    _phantom_response: PhantomData<Response>,
}

unsafe impl<Request: Send, Response: Send> Send for Channel<Request, Response> {}
unsafe impl<Request: Sync, Response: Sync> Sync for Channel<Request, Response> {}

impl<Request, Response> Channel<Request, Response>
where
    Request: for<'a> Decoder<'a> + Send + 'static,
    Response: Encoder + Send + 'static,
{
    /// Create a new channel with a paired sender.
    ///
    /// Returns a tuple of (ChannelSender, Channel). The sender should be
    /// returned to Elixir, and the channel is used by the async task.
    ///
    /// This is typically called automatically by the `#[rustler::task]` macro.
    #[doc(hidden)]
    pub fn new(pid: LocalPid) -> (ChannelSender<Request>, Self) {
        let (tx, rx) = mpsc::unbounded_channel();

        // Create type-erased sender function that decodes Terms to Request
        let send_fn = Arc::new(move |_env: Env, term: Term| -> NifResult<()> {
            let value: Request = term.decode()?;
            tx.send(value)
                .map_err(|_| Error::RaiseTerm(Box::new("channel_closed")))?;
            Ok(())
        });

        let inner = ChannelSenderInner { send_fn };
        let resource_arc = ResourceArc::new(inner);

        let sender: ChannelSender<Request> = ChannelSender {
            inner: resource_arc,
            _phantom: PhantomData,
        };

        let channel = Channel {
            receiver: rx,
            sender: sender.clone(),
            pid,
            _phantom_response: PhantomData,
        };

        (sender, channel)
    }

    /// Receive the next request from the channel.
    ///
    /// Returns `Some(Request)` if a message was received, or `None` if the channel
    /// has been closed (all senders dropped).
    ///
    /// This is an async function that will wait for a message to arrive.
    ///
    /// # Example
    ///
    /// ```ignore
    /// if let Some(request) = channel.recv().await {
    ///     // Handle request
    ///     channel.send(process(request));
    /// }
    /// ```
    pub async fn recv(&mut self) -> Option<Request> {
        self.receiver.recv().await
    }

    /// Try to receive a request without blocking.
    ///
    /// Returns `Some(Request)` if a message is available, or `None` if the channel
    /// is empty or closed.
    pub fn try_recv(&mut self) -> Option<Request> {
        self.receiver.try_recv().ok()
    }

    /// Send a response message to the calling process.
    ///
    /// The message will be sent as `{channel_sender, response}` where `channel_sender`
    /// is the resource reference returned to Elixir.
    ///
    /// # Example
    ///
    /// ```ignore
    /// channel.send("Progress: 50%".to_string());
    /// ```
    pub fn send(&self, response: Response) {
        let mut env = OwnedEnv::new();
        let sender = self.sender.clone();
        let _ = env.send_and_clear(&self.pid, move |env| (sender, response).encode(env));
    }

    /// Send the final response and complete the task.
    ///
    /// This is used internally by the `#[rustler::task]` macro to send the
    /// task's return value. User code should just return the value normally.
    #[doc(hidden)]
    pub fn finish(self, response: Response) {
        self.send(response);
    }

    /// Get a reference to the channel sender for this channel.
    ///
    /// This can be cloned and passed to other tasks or threads for sending
    /// requests TO the channel.
    pub fn sender(&self) -> &ChannelSender<Request> {
        &self.sender
    }

    /// Get a cloneable response sender that can send responses from spawned tasks.
    ///
    /// This is useful when you need to spawn subtasks that send their own
    /// responses back to Elixir.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let responder = channel.responder();
    /// rustler::spawn(async move {
    ///     responder.send(42);
    /// });
    /// ```
    pub fn responder(&self) -> ResponseSender<Request, Response> {
        ResponseSender {
            sender: self.sender.clone(),
            pid: self.pid,
            _phantom: PhantomData,
        }
    }

    /// Check if the channel is closed (all senders dropped).
    pub fn is_closed(&self) -> bool {
        self.receiver.is_closed()
    }

    /// Get the next request from the channel using the Stream trait.
    ///
    /// This is a convenience method that's equivalent to using the Stream trait
    /// directly. Returns `Some(Request)` if a message was received, or `None` if
    /// the channel has been closed.
    ///
    /// # Example
    ///
    /// ```ignore
    /// while let Some(request) = channel.next().await {
    ///     // Handle request
    ///     channel.send(process(request));
    /// }
    /// ```
    pub async fn next(&mut self) -> Option<Request> {
        self.recv().await
    }
}

// Implement Stream trait for idiomatic async iteration
impl<Request, Response> Stream for Channel<Request, Response>
where
    Request: for<'a> Decoder<'a> + Send + 'static,
    Response: Encoder + Send + 'static,
{
    type Item = Request;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        // SAFETY: We never move the receiver
        let this = unsafe { self.get_unchecked_mut() };
        this.receiver.poll_recv(cx)
    }
}

/// NIF function to send a message to a channel.
///
/// This should be exported as a NIF in your module:
///
/// ```ignore
/// #[rustler::nif]
/// fn channel_send_string(
///     env: Env,
///     sender: rustler::runtime::ChannelSender<String>,
///     message: Term
/// ) -> NifResult<Atom> {
///     rustler::runtime::channel::send(env, sender, message)
/// }
/// ```
pub fn send<T>(env: Env, sender: ChannelSender<T>, message: Term) -> NifResult<crate::types::Atom>
where
    T: for<'a> Decoder<'a> + Send + 'static,
{
    (sender.inner.send_fn)(env, message)?;
    Ok(crate::types::atom::ok())
}
