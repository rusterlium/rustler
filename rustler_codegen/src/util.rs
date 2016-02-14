use ::syntax::ast::{MetaItem, MetaItemKind};
use ::syntax::codemap::{Spanned};
use syntax::attr::AttrMetaMethods;

pub fn get_meta_item_value(meta_item: &MetaItem, name: &str) -> Option<String> {
    match meta_item {
        &Spanned { node: MetaItemKind::List(_, ref items), span: _ } => {
            for ref item in items {
                if item.name() == name {
                    match item.value_str() {
                        Some(value) => return Some(value.to_string()),
                        None => (),
                    }
                }
            }
            None
        }
        _ => None
    }
}
