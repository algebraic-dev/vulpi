//! Virtual file system for the compiler. It's used to store the source code of the files that are
//! being compiled.

use std::path::PathBuf;

use filetime::FileTime;
use vulpi_location::FileId;

pub mod real;

#[derive(Debug)]
pub enum Error {
    NotFound(PathBuf),
    NotFoundId,
    AlreadyExists,
}

/// A virtual file system trait that can be implemented by the user. It's used to store the source
/// code of the files that are being compiled and to store the compiled modules.
pub trait FileSystem {
    type Path : Clone;

    fn load(&mut self, path: Self::Path) -> Result<FileId, Error>;
    fn unload(&mut self, id: FileId) -> Result<(), Error>;
    fn path(&self, id: FileId) -> Result<&Self::Path, Error>;

    fn store(&mut self, id: FileId, content: String) -> Result<(), Error>;
    fn read(&self, id: FileId) -> Result<String, Error>;

    fn create(&mut self, path: Self::Path) -> Result<FileId, Error>;
    fn write(&mut self, id: FileId) -> Result<(), Error>;
    fn delete(&mut self, id: FileId) -> Result<(), Error>;

    fn modification_time(&self, id: Self::Path) -> Result<FileTime, Error>;
}
