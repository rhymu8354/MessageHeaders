use super::HeaderName;

/// This is the enumeration of all the different kinds of errors which this
/// crate generates.
#[derive(Debug, Clone, thiserror::Error, PartialEq)]
pub enum Error {
    /// The attached string was expected to be a header line, but does not
    /// have a colon in it.
    #[error("invalid header line")]
    HeaderLineMissingColon(String),

    /// The attached string is the beginning of a line that was expected to
    /// be a header line.  The header line length limit was exceeded before
    /// the line terminator could be found.
    #[error("header line too long")]
    HeaderLineTooLong(String),

    /// The attached string is the name of a header encountered that has
    /// one or more illegal characters in it.
    #[error("header name contains an illegal character")]
    HeaderNameContainsIllegalCharacter(String),

    /// The header with the attached name has a value containing an illegal
    /// character, found in the attached segment.
    #[error("header value contains an illegal character")]
    HeaderValueContainsIllegalCharacter{name: HeaderName, value_segment: String},

    /// The attached string is a header, or part of a header, that is
    /// too long and cannot be folded to fit.
    #[error("header line could not be folded")]
    HeaderLineCouldNotBeFolded(String),

    /// The attached error occurred during string formatting.
    #[error("error during string format")]
    StringFormat(std::fmt::Error),
}

impl From<std::fmt::Error> for Error {
    fn from(error: std::fmt::Error) -> Self {
        Error::StringFormat(error)
    }
}
