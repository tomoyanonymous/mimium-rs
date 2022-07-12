use std::{
    env,
    path::{self, PathBuf},
};

#[derive(Debug)]
pub enum Error {
    IoError(std::io::Error),
    FileNotFound(std::io::Error, PathBuf),
    UtfConversionError(std::string::FromUtf8Error),
    PathJoinError(env::JoinPathsError),
}

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

pub fn load(name: String) -> Result<(String, PathBuf), Error> {
    let cwd = env::current_dir()?;
    let name = path::Path::new(name.as_str());
    let fullpath = cwd.join(name.to_path_buf());

    let fullpath_can = PathBuf::from(fullpath.clone()).canonicalize()?;
    let content =
        std::fs::read(fullpath).map_err(|e| Error::FileNotFound(e, fullpath_can.clone()))?;

    let content_r = String::from_utf8(content).map_err(|e| Error::from(e))?;
    Ok((content_r, fullpath_can))
}
