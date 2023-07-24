use std::fs::DirEntry;

/// Splits a dir entry into a name and it's extension.
pub fn split_name(file: &DirEntry) -> (String, String) {
    let path = file.path();
    (
        path.file_prefix().unwrap().to_string_lossy().to_string(),
        path.extension().unwrap().to_string_lossy().to_string(),
    )
}
