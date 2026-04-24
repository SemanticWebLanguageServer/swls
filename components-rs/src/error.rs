//! Shared error type for all fallible operations in this crate.

use thiserror::Error;

#[derive(Debug, Error)]
pub enum ComponentsJsError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON parse error in {path}: {source}")]
    JsonParse {
        path: String,
        source: serde_json::Error,
    },

    #[error("Semver error for version '{version}': {message}")]
    Semver { version: String, message: String },

    #[error("Context resolution error: {0}")]
    ContextResolution(String),

    #[error("Missing required field '{field}' in {location}")]
    MissingField { field: String, location: String },

    #[error("Invalid URL: {0}")]
    InvalidUrl(String),

    #[error("{0}")]
    General(String),
}

pub type Result<T> = std::result::Result<T, ComponentsJsError>;
