#[macro_export]
macro_rules! resource_struct_def {
    ($name:ident, $typ_field:ident) =>  (
        static mut $typ_field: Option<$crate::resource::NifStructResourceType<$name>> = None;
    )
}

#[macro_export]
macro_rules! resource_struct_init {
    ($name:ident, $typ_field:ident, $env:ident, $nif_typ_name:expr) => (
        {
            let res = match $crate::resource::open_struct_resource_type::<$name>($env, $nif_typ_name, 
                                                                                 $crate::ErlNifResourceFlags::ERL_NIF_RT_CREATE) {
                Some(inner) => inner,
                None => {
                    println!("Failiure in creating resource type");
                    return false;
                },
            };
            unsafe { 
                $typ_field = Some(res) 
            };
        }
    )
}
