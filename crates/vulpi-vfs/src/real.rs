use std::{collections::HashMap, fs, path::PathBuf};

use vulpi_location::FileId;

use super::{Error, FileSystem};

pub struct RealFileSystem {
    root: PathBuf,
    file_map: HashMap<FileId, (PathBuf, String)>,
    path_map: HashMap<PathBuf, FileId>,
    counter: usize,
}

impl RealFileSystem {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            file_map: HashMap::new(),
            path_map: HashMap::new(),
            counter: 0,
        }
    }

    pub fn get_path(&self, path: PathBuf) -> Result<PathBuf, Error> {
        let path = &self.root.clone().join(path);
        path.canonicalize()
            .map_err(|_| Error::NotFound(path.clone()))
    }
}

impl FileSystem<PathBuf, String> for RealFileSystem {
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

    fn store(&mut self, id: FileId, content: String) -> Result<(), Error> {
        let file = self.file_map.get_mut(&id).ok_or(Error::NotFoundId)?;
        *file = (file.0.clone(), content);
        Ok(())
    }

    fn read(&self, id: FileId) -> Result<&String, Error> {
        let file = self.file_map.get(&id).ok_or(Error::NotFoundId)?;
        Ok(&file.1)
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
}
