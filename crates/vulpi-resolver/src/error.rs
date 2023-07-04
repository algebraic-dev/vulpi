use vulpi_location::Location;

pub struct ResolverError {
    pub location: Location,
}

pub type Result<T> = Option<T>;
