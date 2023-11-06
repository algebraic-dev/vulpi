use vulpi_vfs::FileSystem;

pub struct ProjectCompiler<IO: FileSystem> {
    file_system: IO,
}
