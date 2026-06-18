use crate::parser::{ApiDecl, CType};

pub(crate) fn apply_api_signature_fixes(decl: &mut ApiDecl) {
    match decl.name.as_str() {
        "enif_make_map_from_arrays" => {
            decl.make_arg_const(1);
            decl.make_arg_const(2);
        }
        "enif_is_process_alive" => {
            decl.make_arg_const(1);
        }
        "enif_alloc_resource" | "enif_release_resource" | "enif_keep_resource" => {
            decl.make_arg_const(0);
        }
        "enif_make_resource" | "enif_make_resource_binary" => {
            decl.make_arg_const(1);
        }
        "enif_get_resource" => {
            decl.make_arg_const(2);
            decl.update_arg(3, |arg| {
                // void** to *mut *const c_void
                if let CType::Pointer {
                    ref mut pointee, ..
                } = arg.ty
                {
                    pointee.make_const();
                }
            })
        }
        "enif_schedule_nif" => {
            decl.update_arg(3, |arg| {
                // "dereference" as the function parameter is not optional in this case
                if let CType::Pointer { pointee, .. } = arg.ty.clone() {
                    arg.ty = *pointee;
                }
            });
        }
        "enif_monitor_process" | "enif_demonitor_process" => {
            decl.make_arg_const(1);
        }
        "enif_open_resource_type" | "enif_open_resource_type_x" | "enif_init_resource_type" => {
            decl.ret.make_const();
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::apply_api_signature_fixes;
    use crate::parser::{ApiArg, ApiDecl, CBaseType, CPrimitiveType, CType};

    fn primitive(primitive: CPrimitiveType) -> CType {
        CType::Base {
            base: CBaseType::Primitive(primitive),
            is_const: false,
        }
    }

    fn named(name: &str) -> CType {
        CType::Base {
            base: CBaseType::Named(name.to_string()),
            is_const: false,
        }
    }

    fn mut_ptr(pointee: CType) -> CType {
        CType::Pointer {
            pointee: Box::new(pointee),
            is_const: false,
        }
    }

    #[test]
    fn fixes_const_pointer_arguments_and_returns() {
        let mut alloc_resource = ApiDecl {
            ret: mut_ptr(primitive(CPrimitiveType::Void)),
            name: "enif_alloc_resource".into(),
            args: vec![
                ApiArg {
                    name: "type".into(),
                    ty: mut_ptr(named("ErlNifResourceType")),
                },
                ApiArg {
                    name: "size".into(),
                    ty: named("size_t"),
                },
            ],
            variadic: false,
        };
        apply_api_signature_fixes(&mut alloc_resource);
        assert_eq!(
            alloc_resource.args[0].ty,
            CType::Pointer {
                pointee: Box::new(CType::Base {
                    base: CBaseType::Named("ErlNifResourceType".into()),
                    is_const: true,
                }),
                is_const: false,
            }
        );

        let mut open_resource_type = ApiDecl {
            ret: mut_ptr(named("ErlNifResourceType")),
            name: "enif_open_resource_type".into(),
            args: vec![],
            variadic: false,
        };
        apply_api_signature_fixes(&mut open_resource_type);
        assert_eq!(
            open_resource_type.ret,
            CType::Pointer {
                pointee: Box::new(CType::Base {
                    base: CBaseType::Named("ErlNifResourceType".into()),
                    is_const: true,
                }),
                is_const: false,
            }
        );
    }
}
