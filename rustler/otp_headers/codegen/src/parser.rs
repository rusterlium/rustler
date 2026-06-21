use lang_c::ast::{
    Declaration, DeclarationSpecifier, Declarator, DeclaratorKind, DerivedDeclarator, Ellipsis,
    FunctionDeclarator, ParameterDeclaration, PointerQualifier, TypeQualifier, TypeSpecifier,
};
use lang_c::driver::Config;
use lang_c::driver::Flavor::StdC11;
use lang_c::visit::Visit;

#[derive(Debug, Clone)]
pub(crate) struct ApiArg {
    pub name: String,
    pub ty: CType,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CParam {
    pub name: Option<String>,
    pub ty: CType,
}

#[derive(Debug)]
pub(crate) struct ApiDecl {
    pub ret: CType,
    pub name: String,
    pub args: Vec<ApiArg>,
    pub variadic: bool,
}

impl ApiDecl {
    pub(crate) fn update_arg<F: FnOnce(&mut ApiArg)>(&mut self, i: usize, f: F) {
        self.args.get_mut(i).map(f);
    }

    pub(crate) fn make_arg_const(&mut self, i: usize) {
        self.update_arg(i, |arg| arg.ty.make_const());
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CPrimitiveType {
    Void,
    Char,
    UnsignedChar,
    Int,
    UnsignedInt,
    Long,
    UnsignedLong,
    Double,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CBaseType {
    Primitive(CPrimitiveType),
    Named(String),
    Enum(String),
}

impl CBaseType {
    pub(crate) fn is_void(&self) -> bool {
        matches!(self, CBaseType::Primitive(CPrimitiveType::Void))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CType {
    Base {
        base: CBaseType,
        is_const: bool,
    },
    Pointer {
        pointee: Box<CType>,
        is_const: bool,
    },
    Function {
        ret: Box<CType>,
        params: Vec<CParam>,
        variadic: bool,
    },
}

impl CType {
    pub(crate) fn is_const_qualified(&self) -> bool {
        match self {
            CType::Base { is_const, .. } => *is_const,
            CType::Pointer { is_const, .. } => *is_const,
            CType::Function { .. } => false,
        }
    }

    pub(crate) fn make_const(&mut self) {
        match self {
            CType::Base { is_const, .. } => *is_const = true,
            CType::Pointer { pointee, .. } => pointee.make_const(),
            CType::Function { .. } => {}
        }
    }
}

pub(crate) fn parse_api_declarations_source(source: &str) -> Vec<ApiDecl> {
    let config = Config {
        flavor: StdC11,
        ..Default::default()
    };

    let parsed = lang_c::driver::parse_preprocessed(&config, source.to_owned())
        .expect("Failed to parse C declarations");

    collect_api_decls_from_c_ast(&parsed.unit)
}

#[derive(Default)]
struct BaseTypeCollector {
    is_const: bool,
    specifiers: Vec<TypeSpecifier>,
    base: Option<CBaseType>,
}

impl BaseTypeCollector {
    fn into_c_type(self) -> CType {
        let base = self
            .base
            .unwrap_or_else(|| CBaseType::Primitive(fold_primitive_type(&self.specifiers)));
        CType::Base {
            base,
            is_const: self.is_const,
        }
    }
}

impl<'a> Visit<'a> for BaseTypeCollector {
    fn visit_declaration_specifier(
        &mut self,
        declaration_specifier: &'a DeclarationSpecifier,
        _span: &'a lang_c::span::Span,
    ) {
        match declaration_specifier {
            DeclarationSpecifier::StorageClass(_) => {}
            DeclarationSpecifier::TypeQualifier(type_qualifier) => {
                self.visit_type_qualifier(&type_qualifier.node, &type_qualifier.span);
            }
            DeclarationSpecifier::TypeSpecifier(type_specifier) => {
                self.visit_type_specifier(&type_specifier.node, &type_specifier.span);
            }
            DeclarationSpecifier::Extension(_) => {}
            _ => panic!("Unsupported declaration specifier in API declarations"),
        }
    }

    fn visit_type_qualifier(
        &mut self,
        type_qualifier: &'a TypeQualifier,
        _span: &'a lang_c::span::Span,
    ) {
        match type_qualifier {
            TypeQualifier::Const => self.is_const = true,
            _ => panic!("Unsupported type qualifier in API declarations"),
        }
    }

    fn visit_type_specifier(
        &mut self,
        type_specifier: &'a TypeSpecifier,
        _span: &'a lang_c::span::Span,
    ) {
        match type_specifier {
            TypeSpecifier::Void
            | TypeSpecifier::Char
            | TypeSpecifier::Int
            | TypeSpecifier::Long
            | TypeSpecifier::Double
            | TypeSpecifier::Unsigned => self.specifiers.push(type_specifier.clone()),
            TypeSpecifier::TypedefName(ident) => {
                self.base = Some(CBaseType::Named(ident.node.name.clone()));
            }
            TypeSpecifier::Enum(en) => {
                let identifier = en
                    .node
                    .identifier
                    .as_ref()
                    .unwrap_or_else(|| panic!("Unsupported anonymous enum in API declarations"))
                    .node
                    .name
                    .clone();
                self.base = Some(CBaseType::Enum(identifier));
            }
            _ => panic!("Unsupported type specifier in API declarations"),
        }
    }
}

struct DeclCollector {
    pub result: Vec<ApiDecl>,
}

impl DeclCollector {
    fn new() -> Self {
        DeclCollector { result: vec![] }
    }

    fn release(self) -> Vec<ApiDecl> {
        self.result
    }
}

impl<'a> lang_c::visit::Visit<'a> for DeclCollector {
    fn visit_declaration(&mut self, decl: &'a Declaration, _: &'a lang_c::span::Span) {
        if let Some(res) = extract_api_decl_from_declaration(decl) {
            self.result.push(res);
        }
    }
}

fn collect_api_decls_from_c_ast(unit: &lang_c::ast::TranslationUnit) -> Vec<ApiDecl> {
    let mut coll = DeclCollector::new();
    coll.visit_translation_unit(unit);
    coll.release()
}

fn extract_api_decl_from_declaration(decl: &Declaration) -> Option<ApiDecl> {
    if decl
        .specifiers
        .iter()
        .any(|specifier| matches!(specifier.node, DeclarationSpecifier::StorageClass(_)))
    {
        return None;
    }

    let mut out = None;
    for declarator in &decl.declarators {
        let (name, c_type) = apply_declarator(
            base_type_from_decl_specifiers(&decl.specifiers),
            &declarator.node.declarator,
        );
        let CType::Function {
            ret,
            params,
            variadic,
        } = c_type
        else {
            continue;
        };
        let Some(name) = name else {
            continue;
        };
        if !name.starts_with("enif_") {
            continue;
        }

        let mut args = Vec::new();
        if !(params.len() == 1
            && matches!(
                params.first().map(|param| &param.ty),
                Some(CType::Base {
                    base,
                    is_const: false
                }) if base.is_void()
            ))
        {
            for (idx, param) in params.into_iter().enumerate() {
                args.push(ApiArg {
                    name: param.name.unwrap_or_else(|| format!("arg{}", idx + 1)),
                    ty: param.ty,
                });
            }
        }

        out = Some(ApiDecl {
            ret: *ret,
            name,
            args,
            variadic,
        });
    }
    out
}

fn base_type_from_decl_specifiers(
    specifiers: &[lang_c::span::Node<DeclarationSpecifier>],
) -> CType {
    let mut collector = BaseTypeCollector::default();
    for spec in specifiers {
        collector.visit_declaration_specifier(&spec.node, &spec.span);
    }
    collector.into_c_type()
}

fn fold_primitive_type(specifiers: &[TypeSpecifier]) -> CPrimitiveType {
    match specifiers {
        [TypeSpecifier::Void] => CPrimitiveType::Void,
        [TypeSpecifier::Char] => CPrimitiveType::Char,
        [TypeSpecifier::Unsigned, TypeSpecifier::Char]
        | [TypeSpecifier::Char, TypeSpecifier::Unsigned] => CPrimitiveType::UnsignedChar,
        [TypeSpecifier::Int] => CPrimitiveType::Int,
        [TypeSpecifier::Unsigned] | [TypeSpecifier::Unsigned, TypeSpecifier::Int] => {
            CPrimitiveType::UnsignedInt
        }
        [TypeSpecifier::Long] => CPrimitiveType::Long,
        [TypeSpecifier::Unsigned, TypeSpecifier::Long]
        | [TypeSpecifier::Long, TypeSpecifier::Unsigned] => CPrimitiveType::UnsignedLong,
        [TypeSpecifier::Double] => CPrimitiveType::Double,
        _ => panic!("Unsupported primitive C type: {:?}", specifiers),
    }
}

fn pointer_has_const_qualifier(qualifiers: &[lang_c::span::Node<PointerQualifier>]) -> bool {
    qualifiers.iter().any(|qualifier| {
        matches!(
            qualifier.node,
            PointerQualifier::TypeQualifier(ref q) if q.node == TypeQualifier::Const
        )
    })
}

fn apply_declarator(
    base: CType,
    declarator: &lang_c::span::Node<Declarator>,
) -> (Option<String>, CType) {
    fn apply_derived(
        mut ty: CType,
        derived_list: &[lang_c::span::Node<DerivedDeclarator>],
    ) -> CType {
        for derived in derived_list {
            ty = match &derived.node {
                DerivedDeclarator::Pointer(qualifiers) => CType::Pointer {
                    pointee: Box::new(ty),
                    is_const: pointer_has_const_qualifier(qualifiers),
                },
                DerivedDeclarator::Array(_) => CType::Pointer {
                    pointee: Box::new(ty),
                    is_const: false,
                },
                DerivedDeclarator::Function(function) => {
                    let (params, variadic) = parse_function_params(function);
                    CType::Function {
                        ret: Box::new(ty),
                        params,
                        variadic,
                    }
                }
                DerivedDeclarator::KRFunction(_) => {
                    panic!("Unsupported K&R function declarator in API declarations")
                }
                DerivedDeclarator::Block(_) => {
                    panic!("Unsupported block declarator in API declarations")
                }
            };
        }
        ty
    }

    match &declarator.node.kind.node {
        DeclaratorKind::Identifier(identifier) => (
            Some(identifier.node.name.clone()),
            apply_derived(base, &declarator.node.derived),
        ),
        DeclaratorKind::Abstract => (None, apply_derived(base, &declarator.node.derived)),
        DeclaratorKind::Declarator(inner) => {
            let outer = apply_derived(base, &declarator.node.derived);
            apply_declarator(outer, inner)
        }
    }
}

#[derive(Default)]
struct FunctionParamsCollector {
    params: Vec<CParam>,
    variadic: bool,
}

impl<'a> Visit<'a> for FunctionParamsCollector {
    fn visit_parameter_declaration(
        &mut self,
        parameter_declaration: &'a ParameterDeclaration,
        _span: &'a lang_c::span::Span,
    ) {
        self.params.push(parse_parameter(parameter_declaration));
    }

    fn visit_ellipsis(&mut self, ellipsis: &'a Ellipsis, _span: &'a lang_c::span::Span) {
        self.variadic = matches!(ellipsis, Ellipsis::Some);
    }
}

fn parse_function_params(function: &lang_c::span::Node<FunctionDeclarator>) -> (Vec<CParam>, bool) {
    let mut collector = FunctionParamsCollector::default();
    collector.visit_function_declarator(&function.node, &function.span);
    (collector.params, collector.variadic)
}

fn parse_parameter(param: &ParameterDeclaration) -> CParam {
    let base = base_type_from_decl_specifiers(&param.specifiers);
    if let Some(declarator) = &param.declarator {
        let (name, ty) = apply_declarator(base, declarator);
        CParam { name, ty }
    } else {
        CParam {
            name: None,
            ty: base,
        }
    }
}
