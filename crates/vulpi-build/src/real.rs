use std::{collections::HashMap, fs, path::PathBuf};

use filetime::FileTime;
use vulpi_intern::Symbol;
use vulpi_location::FileId;
use vulpi_vfs::{Error, path::Path};

use super::FileSystem;

pub struct RealFileSystem {
    project_root: PathBuf,
    build_root: PathBuf,
    root: Symbol,
    file_map: HashMap<FileId, (PathBuf, String)>,
    path_map: HashMap<PathBuf, FileId>,
    counter: usize,
}

impl RealFileSystem {
    pub fn new(root: Symbol, project_root: PathBuf, build: PathBuf) -> Self {
        Self {
            root,
            project_root,
            build_root: build,
            file_map: HashMap::new(),
            path_map: HashMap::new(),
            counter: 0,
        }
    }

    pub fn get_path(&self, path: PathBuf) -> Result<PathBuf, Error> {
        let path = &self.project_root.clone().join(path);
        path.canonicalize()
            .map_err(|_| Error::NotFound(path.clone()))
    }
}

impl FileSystem for RealFileSystem {
    type Path = PathBuf;

    fn load(&mut self, path: PathBuf) -> Result<FileId, Error> {
        let path = self.get_path(path)?;

        if let Some(id) = self.path_map.get(&path) {
            return Ok(*id);
        }

        let content =
            fs::read_to_string(path.clone()).map_err(|_| Error::NotFound(path.clone()))?;

        let id = FileId(self.counter);
        self.counter += 1;

        let content = (path.clone(), content);

        self.file_map.insert(id, content);
        self.path_map.insert(path, id);

        Ok(id)
    }

    fn unload(&mut self, id: FileId) -> Result<(), Error> {
        self.file_map.remove(&id).ok_or(Error::NotFoundId)?;
        Ok(())
    }

    fn store(&mut self, _id: FileId, _content: String) -> Result<(), Error> {
        todo!()
    }

    fn read(&self, id: FileId) -> Result<String, Error> {
        let file = self.file_map.get(&id).ok_or(Error::NotFoundId)?;
        Ok(file.1.clone())
    }

    fn create(&mut self, path: PathBuf) -> Result<FileId, Error> {
        let path = self.get_path(path)?;

        if path.exists() {
            return Err(Error::AlreadyExists);
        }

        let id = FileId(self.counter);
        self.counter += 1;

        self.file_map.insert(id, (path.clone(), String::new()));
        self.path_map.insert(path, id);

        Ok(id)
    }

    fn write(&mut self, id: FileId) -> Result<(), Error> {
        if let Some((path, content)) = self.file_map.get(&id) {
            fs::write(path, content).map_err(|_| Error::NotFound(path.clone()))?;
            Ok(())
        } else {
            Err(Error::NotFoundId)
        }
    }

    fn delete(&mut self, id: FileId) -> Result<(), Error> {
        if let Some((path, _)) = self.file_map.get(&id) {
            fs::remove_file(path).map_err(|_| Error::NotFound(path.clone()))?;
            Ok(())
        } else {
            Err(Error::NotFoundId)
        }
    }

    fn path(&self, id: FileId) -> Result<&PathBuf, Error> {
        let file = self.file_map.get(&id).ok_or(Error::NotFoundId)?;
        Ok(&file.0)
    }

    fn modification_time(&self, path: PathBuf) -> Result<FileTime, Error> {
        let metadata = fs::metadata(path.clone()).map_err(|_| Error::NotFound(path.clone()))?;

        Ok(FileTime::from_last_modification_time(&metadata))
    }

    fn from_cached_path(&self, path: Path) -> Self::Path {
        path.to_pathbuf(self.build_root.clone())
    }

    fn from_src_path(&self, path: Path) -> Self::Path {
        if self.root == path.segments[0] {
            path.shift().to_pathbuf(self.project_root.clone())
        } else {
            path.to_pathbuf(self.project_root.clone())
        }
    }
}
