/// Indicates that a resource has not been registered successfully
#[derive(Clone, Copy, Debug)]
pub struct ResourceInitError;

/// Indicates that a dynamic resource call failed
#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub struct DynamicResourceCallError;
