use crate::{Env, MonitorResource, Resource};

use super::{Registration, ResourceInitError};

impl<'a> Env<'a> {
    pub unsafe fn to_init_env(&mut self) {
        self.init = true;
    }

    pub fn add_resource_type<T: Resource>(self) -> Result<(), ResourceInitError> {
        if !self.init {
            return Err(ResourceInitError);
        }

        Registration::new::<T>().register(self)
    }

    pub fn add_monitor_resource_type<T: MonitorResource>(self) -> Result<(), ResourceInitError> {
        if !self.init {
            return Err(ResourceInitError);
        }

        Registration::new::<T>()
            .add_down_callback::<T>()
            .register(self)
    }
}
