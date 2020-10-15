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

    /// The attached string is the value of a header encountered that has
    /// one or more illegal characters in it.
    ///
    /// TODO: Consider including header name here as well
    #[error("header value contains an illegal character")]
    HeaderValueContainsIllegalCharacter(String),
}

impl From<Error> for std::fmt::Error {
    fn from(_: Error) -> Self {
        Self{}
    }
}