use crate::sys::{enif_compare_monitors, ErlNifMonitor};

/// Handle for a monitor created using `ResourceArc<T>::monitor`.
///
/// A monitor handle can be compared to other monitor handles. It is opaque and freely copyable.
/// The monitor will not become inactive if this object is dropped.
#[derive(Copy, Clone)]
pub struct Monitor {
    inner: ErlNifMonitor,
}

impl Monitor {
    pub unsafe fn new(inner: ErlNifMonitor) -> Self {
        Self { inner }
    }

    pub fn as_c_arg(&self) -> &ErlNifMonitor {
        &self.inner
    }

    pub fn from_c_arg(erl_nif_mon: ErlNifMonitor) -> Self {
        Monitor { inner: erl_nif_mon }
    }
}

impl PartialEq for Monitor {
    fn eq(&self, other: &Self) -> bool {
        unsafe { enif_compare_monitors(&self.inner, &other.inner) == 0 }
    }
}
