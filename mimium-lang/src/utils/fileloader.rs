use std::{env, fmt, path::PathBuf};

#[derive(Debug)]
pub enum Error {
    IoError(std::io::Error),
    FileNotFound(std::io::Error, PathBuf),
    UtfConversionError(std::string::FromUtf8Error),
    PathJoinError(env::JoinPathsError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::IoError(e) => write!(f, "IoError{:?}", e),
            Error::FileNotFound(e, p) => write!(f, "File {} not found: {:?}", p.display(), e),
            Error::UtfConversionError(e) => write!(f, "Failed to convert into UTF{:?}", e),
            Error::PathJoinError(e) => write!(f, "Failed to join paths{:?}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IoError(e)
    }
}
impl From<std::string::FromUtf8Error> for Error {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Error::UtfConversionError(e)
    }
}
impl From<env::JoinPathsError> for Error {
    fn from(e: env::JoinPathsError) -> Self {
        Error::PathJoinError(e)
    }
}
fn get_default_library_path() -> Option<PathBuf> {
    let home = homedir::my_home().ok().flatten();
    if home.is_none() {
        log::warn!("default library search path is not available on this platform.");
        return None;
    }
    let p = home.unwrap().join(PathBuf::from(".mimium/lib"));
    Some(p)
}

pub fn get_canonical_path(current_file_or_dir: &str, relpath: &str) -> Result<PathBuf, Error> {
    let parent_dir = get_parent_dir(current_file_or_dir)?;
    let relpath2 = std::path::PathBuf::from(relpath);
    let abspath = [parent_dir, relpath2]
        .into_iter()
        .collect::<std::path::PathBuf>();
    if cfg!(target_arch = "wasm32") {
        //canonicalize is platform-dependent and always returns Err on wasm32
        Ok(abspath)
    } else {
        abspath.canonicalize().map_err(Error::IoError)
    }
}

fn get_parent_dir(current_file: &str) -> Result<PathBuf, Error> {
    let current_filepath = std::path::Path::new(current_file);
    if current_filepath.is_dir() {
        Ok(current_filepath.into())
    } else {
        #[cfg(not(target_arch = "wasm32"))]
        let cwd = env::current_dir()?;
        #[cfg(target_arch = "wasm32")]
        let cwd = std::path::PathBuf::new();
        Ok(current_filepath.parent().map_or_else(|| cwd, PathBuf::from))
    }
}

/// Used for resolving include.
/// If the filename is given it searches ~/.mimium/lib first. If not found, tries to find in relative path.
/// If the relative path is given explicitly, do not find in standard library path.

pub fn load_mmmlibfile(current_file_or_dir: &str, path: &str) -> Result<(String, PathBuf), Error> {
    let path = std::path::Path::new(path);
    let search_default_lib = !(path.is_absolute() || path.starts_with("."));
    if let (true, Some(stdlibpath)) = (search_default_lib, get_default_library_path()) {
        let cpath = stdlibpath.join(path).canonicalize()?;
        let content = load(&cpath.to_string_lossy())?;
        return Ok((content, cpath));
    };
    let cpath = get_canonical_path(current_file_or_dir, &path.to_string_lossy())?;
    let content = load(&cpath.to_string_lossy())?;
    Ok((content, cpath))
}

pub fn load(canonical_path: &str) -> Result<String, Error> {
    // debug_assert!(std::path::Path::new(canonical_path).is_absolute());
    let content = std::fs::read(canonical_path)
        .map_err(|e| Error::FileNotFound(e, PathBuf::from(canonical_path)))?;

    let content_r = String::from_utf8(content).map_err(Error::from)?;
    Ok(content_r)
}
