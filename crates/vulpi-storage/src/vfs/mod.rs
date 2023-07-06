//! Virtual file system for the compiler. It's used to store the source code of the files that are
//! being compiled.

use std::path::PathBuf;

use crate::id::{File, Id};

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
    fn load(&mut self, path: Path) -> Result<Id<File>, Error>;
    fn unload(&mut self, id: Id<File>) -> Result<(), Error>;
    fn path(&self, id: Id<File>) -> Result<&Path, Error>;

    fn store(&mut self, id: Id<File>, content: String) -> Result<(), Error>;
    fn read(&self, id: Id<File>) -> Result<&Content, Error>;

    fn create(&mut self, path: Path) -> Result<Id<File>, Error>;
    fn write(&mut self, id: Id<File>) -> Result<(), Error>;
    fn delete(&mut self, id: Id<File>) -> Result<(), Error>;
}
