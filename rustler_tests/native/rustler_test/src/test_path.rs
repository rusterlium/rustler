use std::path::{Path, PathBuf};

#[rustler::nif]
pub fn append_to_path(p: &Path, comp: &str) -> PathBuf {
    let mut p = p.to_path_buf();
    p.push(comp);
    p
}
