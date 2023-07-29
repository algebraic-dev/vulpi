//! Virtual file system for the compiler. It's used to store the source code of the files that are
//! being compiled.

use std::path::PathBuf;

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
pub trait FileSystem<Path, Content> {
    fn load(&mut self, path: Path) -> Result<FileId, Error>;
    fn unload(&mut self, id: FileId) -> Result<(), Error>;
    fn path(&self, id: FileId) -> Result<&Path, Error>;

    fn store(&mut self, id: FileId, content: String) -> Result<(), Error>;
    fn read(&self, id: FileId) -> Result<&Content, Error>;

    fn create(&mut self, path: Path) -> Result<FileId, Error>;
    fn write(&mut self, id: FileId) -> Result<(), Error>;
    fn delete(&mut self, id: FileId) -> Result<(), Error>;
}
