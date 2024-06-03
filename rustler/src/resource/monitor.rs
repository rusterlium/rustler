use rustler_sys::ErlNifMonitor;

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
        unsafe { rustler_sys::enif_compare_monitors(&self.inner, &other.inner) == 0 }
    }
}
