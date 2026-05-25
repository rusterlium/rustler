macro_rules! nif_callback_type {
    (fn($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?) => {
        Option<extern "C" fn($($arg_ty),*) $(-> $ret)?>
    };
    (variadic($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?) => {
        Option<extern "C" fn($($arg_ty),*, ...) $(-> $ret)?>
    };
    (dummy) => {
        Option<extern "C" fn()>
    };
}

macro_rules! nif_forwarder_item {
    ([$($meta:tt)*] fn $name:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?;) => {
        $($meta)*
        #[doc = concat!(
            "See [",
            stringify!($name),
            "](http://www.erlang.org/doc/man/erl_nif.html#",
            stringify!($name),
            ") in the Erlang docs."
        )]
        #[inline]
        pub unsafe extern "C" fn $name($($arg_name: $arg_ty),*) $(-> $ret)? {
            (DYN_NIF_CALLBACKS.$name.unwrap_unchecked())($($arg_name),*)
        }
    };
    ([$($meta:tt)*] variadic fn $name:ident => $getter:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?;) => {
        $($meta)*
        pub unsafe fn $getter() -> extern "C" fn($($arg_ty),*, ...) $(-> $ret)? {
            DYN_NIF_CALLBACKS.$name.unwrap_unchecked()
        }
    };
    ([$($meta:tt)*] dummy $name:ident;) => {};
}

macro_rules! nif_writer_stmt {
    ($self_ident:ident $filler_ident:ident [$($meta:tt)*] fn $name:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?;) => {
        $($meta)*
        $filler_ident.write(&mut $self_ident.$name, concat!(stringify!($name), "\0"));
    };
    ($self_ident:ident $filler_ident:ident [$($meta:tt)*] variadic fn $name:ident => $getter:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?;) => {
        $($meta)*
        $filler_ident.write(&mut $self_ident.$name, concat!(stringify!($name), "\0"));
    };
    ($self_ident:ident $filler_ident:ident [$($meta:tt)*] dummy $name:ident;) => {};
}

macro_rules! define_nif_callbacks {
    (@accum [$($fields:tt)*]) => {
        #[allow(dead_code)]
        #[derive(Default, Copy, Clone)]
        pub struct DynNifCallbacks {
            $($fields)*
        }
    };
    (@accum [$($fields:tt)*] $(#[$meta:meta])+ { $($inner:tt)* } $($rest:tt)*) => {
        define_nif_callbacks!(@group [$($fields)*] [$(#[$meta])*] [$($rest)*] $($inner)*);
    };
    (@accum [$($fields:tt)*] $(#[$meta:meta])* fn $name:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($rest:tt)*) => {
        define_nif_callbacks!(
            @accum [
                $($fields)*
                $(#[$meta])*
                $name: nif_callback_type!(fn($($arg_name: $arg_ty),*) $(-> $ret)?),
            ]
            $($rest)*
        );
    };
    (@accum [$($fields:tt)*] $(#[$meta:meta])* variadic fn $name:ident => $getter:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($rest:tt)*) => {
        define_nif_callbacks!(
            @accum [
                $($fields)*
                $(#[$meta])*
                $name: nif_callback_type!(variadic($($arg_name: $arg_ty),*) $(-> $ret)?),
            ]
            $($rest)*
        );
    };
    (@accum [$($fields:tt)*] $(#[$meta:meta])* dummy $name:ident; $($rest:tt)*) => {
        define_nif_callbacks!(
            @accum [
                $($fields)*
                $(#[$meta])*
                $name: nif_callback_type!(dummy),
            ]
            $($rest)*
        );
    };
    (@group [$($fields:tt)*] [$($group_meta:tt)*] [$($rest:tt)*]) => {
        define_nif_callbacks!(@accum [$($fields)*] $($rest)*);
    };
    (@group [$($fields:tt)*] [$($group_meta:tt)*] [$($rest:tt)*] $(#[$meta:meta])* fn $name:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($inner_rest:tt)*) => {
        define_nif_callbacks!(
            @group [
                $($fields)*
                $($group_meta)*
                $(#[$meta])*
                $name: nif_callback_type!(fn($($arg_name: $arg_ty),*) $(-> $ret)?),
            ]
            [$($group_meta)*]
            [$($rest)*]
            $($inner_rest)*
        );
    };
    (@group [$($fields:tt)*] [$($group_meta:tt)*] [$($rest:tt)*] $(#[$meta:meta])* variadic fn $name:ident => $getter:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($inner_rest:tt)*) => {
        define_nif_callbacks!(
            @group [
                $($fields)*
                $($group_meta)*
                $(#[$meta])*
                $name: nif_callback_type!(variadic($($arg_name: $arg_ty),*) $(-> $ret)?),
            ]
            [$($group_meta)*]
            [$($rest)*]
            $($inner_rest)*
        );
    };
    (@group [$($fields:tt)*] [$($group_meta:tt)*] [$($rest:tt)*] $(#[$meta:meta])* dummy $name:ident; $($inner_rest:tt)*) => {
        define_nif_callbacks!(
            @group [
                $($fields)*
                $($group_meta)*
                $(#[$meta])*
                $name: nif_callback_type!(dummy),
            ]
            [$($group_meta)*]
            [$($rest)*]
            $($inner_rest)*
        );
    };
    ($($items:tt)*) => {
        define_nif_callbacks!(@accum [] $($items)*);
    };
}

macro_rules! define_nif_forwarders {
    () => {};
    ($(#[$meta:meta])+ { $($inner:tt)* } $($rest:tt)*) => {
        define_nif_forwarders!(@group [$(#[$meta])*] [$($rest)*] $($inner)*);
    };
    ($(#[$meta:meta])* fn $name:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($rest:tt)*) => {
        nif_forwarder_item!([$(#[$meta])*] fn $name($($arg_name: $arg_ty),*) $(-> $ret)?;);
        define_nif_forwarders!($($rest)*);
    };
    ($(#[$meta:meta])* variadic fn $name:ident => $getter:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($rest:tt)*) => {
        nif_forwarder_item!([$(#[$meta])*] variadic fn $name => $getter($($arg_name: $arg_ty),*) $(-> $ret)?;);
        define_nif_forwarders!($($rest)*);
    };
    ($(#[$meta:meta])* dummy $name:ident; $($rest:tt)*) => {
        nif_forwarder_item!([$(#[$meta])*] dummy $name;);
        define_nif_forwarders!($($rest)*);
    };
    (@group [$($group_meta:tt)*] [$($rest:tt)*]) => {
        define_nif_forwarders!($($rest)*);
    };
    (@group [$($group_meta:tt)*] [$($rest:tt)*] $(#[$meta:meta])* fn $name:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($inner_rest:tt)*) => {
        nif_forwarder_item!([$($group_meta)* $(#[$meta])*] fn $name($($arg_name: $arg_ty),*) $(-> $ret)?;);
        define_nif_forwarders!(@group [$($group_meta)*] [$($rest)*] $($inner_rest)*);
    };
    (@group [$($group_meta:tt)*] [$($rest:tt)*] $(#[$meta:meta])* variadic fn $name:ident => $getter:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($inner_rest:tt)*) => {
        nif_forwarder_item!([$($group_meta)* $(#[$meta])*] variadic fn $name => $getter($($arg_name: $arg_ty),*) $(-> $ret)?;);
        define_nif_forwarders!(@group [$($group_meta)*] [$($rest)*] $($inner_rest)*);
    };
    (@group [$($group_meta:tt)*] [$($rest:tt)*] $(#[$meta:meta])* dummy $name:ident; $($inner_rest:tt)*) => {
        nif_forwarder_item!([$($group_meta)* $(#[$meta])*] dummy $name;);
        define_nif_forwarders!(@group [$($group_meta)*] [$($rest)*] $($inner_rest)*);
    };
}

macro_rules! define_nif_writers {
    ($self_ident:ident $filler_ident:ident;) => {};
    ($self_ident:ident $filler_ident:ident; $(#[$meta:meta])+ { $($inner:tt)* } $($rest:tt)*) => {
        define_nif_writers!(@group $self_ident $filler_ident [$($rest)*] [$(#[$meta])*] $($inner)*);
    };
    ($self_ident:ident $filler_ident:ident; $(#[$meta:meta])* fn $name:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($rest:tt)*) => {
        nif_writer_stmt!($self_ident $filler_ident [$(#[$meta])*] fn $name($($arg_name: $arg_ty),*) $(-> $ret)?;);
        define_nif_writers!($self_ident $filler_ident; $($rest)*);
    };
    ($self_ident:ident $filler_ident:ident; $(#[$meta:meta])* variadic fn $name:ident => $getter:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($rest:tt)*) => {
        nif_writer_stmt!($self_ident $filler_ident [$(#[$meta])*] variadic fn $name => $getter($($arg_name: $arg_ty),*) $(-> $ret)?;);
        define_nif_writers!($self_ident $filler_ident; $($rest)*);
    };
    ($self_ident:ident $filler_ident:ident; $(#[$meta:meta])* dummy $name:ident; $($rest:tt)*) => {
        nif_writer_stmt!($self_ident $filler_ident [$(#[$meta])*] dummy $name;);
        define_nif_writers!($self_ident $filler_ident; $($rest)*);
    };
    (@group $self_ident:ident $filler_ident:ident [$($rest:tt)*] [$($group_meta:tt)*]) => {
        define_nif_writers!($self_ident $filler_ident; $($rest)*);
    };
    (@group $self_ident:ident $filler_ident:ident [$($rest:tt)*] [$($group_meta:tt)*] $(#[$meta:meta])* fn $name:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($inner_rest:tt)*) => {
        nif_writer_stmt!($self_ident $filler_ident [$($group_meta)* $(#[$meta])*] fn $name($($arg_name: $arg_ty),*) $(-> $ret)?;);
        define_nif_writers!(@group $self_ident $filler_ident [$($rest)*] [$($group_meta)*] $($inner_rest)*);
    };
    (@group $self_ident:ident $filler_ident:ident [$($rest:tt)*] [$($group_meta:tt)*] $(#[$meta:meta])* variadic fn $name:ident => $getter:ident($($arg_name:ident : $arg_ty:ty),* $(,)?) $(-> $ret:ty)?; $($inner_rest:tt)*) => {
        nif_writer_stmt!($self_ident $filler_ident [$($group_meta)* $(#[$meta])*] variadic fn $name => $getter($($arg_name: $arg_ty),*) $(-> $ret)?;);
        define_nif_writers!(@group $self_ident $filler_ident [$($rest)*] [$($group_meta)*] $($inner_rest)*);
    };
    (@group $self_ident:ident $filler_ident:ident [$($rest:tt)*] [$($group_meta:tt)*] $(#[$meta:meta])* dummy $name:ident; $($inner_rest:tt)*) => {
        nif_writer_stmt!($self_ident $filler_ident [$($group_meta)* $(#[$meta])*] dummy $name;);
        define_nif_writers!(@group $self_ident $filler_ident [$($rest)*] [$($group_meta)*] $($inner_rest)*);
    };
    ($($items:tt)*) => {
        impl DynNifCallbacks {
            fn write_symbols<T: DynNifFiller>(&mut self, filler: T) {
                define_nif_writers!(self filler; $($items)*);
            }
        }
    };
}

macro_rules! define_nif_api {
    ($($items:tt)*) => {
        define_nif_callbacks!($($items)*);
        define_nif_forwarders!($($items)*);
        define_nif_writers!($($items)*);
    };
}
