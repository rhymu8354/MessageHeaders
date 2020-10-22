//! This crate provides types for parsing and generating the headers of
//! Internet messages, such as e-mail, HTTP requests or responses, and SIP
//! messages.  The typical format of these headers is described in [IETF RFC
//! 5322](https://tools.ietf.org/html/rfc5322).  Note that specific Internet
//! standards may leverage this format but make exceptions, extensions, or
//! other changes to it.
//!
//! To parse a message, construct a [`MessageHeaders`] value using
//! [`MessageHeaders::new`], and then start feeding it input text via
//! [`MessageHeaders::parse`].  Each call will consume zero or more of the
//! input characters.  Input is only consumed if enough is provided for the
//! parser to determine when all of one or more headers has been provided.  Any
//! unused input should be included in the next call along with additional
//! input to continue parsing.  Parsing is complete once the message body is
//! found.
//!
//! To generate a message, construct a [`MessageHeaders`] value using
//! [`MessageHeaders::new`], fill in headers using
//! [`MessageHeaders::add_header`] and other related functions, and emit
//! the final text using [`MessageHeaders::generate`].
//!
//! By default, there is no constraint set on the lengths of header lines.
//! This can cause issues in some places, such as servers receiving input from
//! untrusted connections.  An attacker might feed in a constant stream of
//! characters that never includes a line ending, causing the server to exceed
//! its memory allocation limits.  To prevent this, the
//! [`MessageHeaders::set_line_limit`] function can be used to set a constraint
//! for header line length, both for parsing and generation.
//!
//! [`MessageHeaders`]: struct.MessageHeaders.html
//! [`MessageHeaders::new`]: struct.MessageHeaders.html#method.new
//! [`MessageHeaders::parse`]: struct.MessageHeaders.html#method.parse
//! [`MessageHeaders::add_header`]: struct.MessageHeaders.html#method.add_header
//! [`MessageHeaders::generate`]: struct.MessageHeaders.html#method.generate
//! [`MessageHeaders::set_line_limit`]: struct.MessageHeaders.html#method.set_line_limit

#![warn(clippy::pedantic)]
#![warn(missing_docs)]
#![allow(clippy::non_ascii_literal)]

#[cfg(test)]
#[macro_use]
extern crate named_tuple;

mod error;

pub use error::Error;
use std::fmt::Write;

// These are the characters that are considered whitespace and
// should be stripped off by the Strip() function.
//
// Name of "WSP" chosen to match the symbol name from
// RFC 5322 (https://tools.ietf.org/html/rfc5322) which refers
// to this specific character set.
const WSP: &str = " \t";

// This is the required line terminator for internet message header lines.
const CRLF: &str = "\r\n";

fn find_crlf<T>(message: T) -> Option<usize>
    where T: AsRef<[u8]>
{
    message.as_ref().windows(2)
        .enumerate()
        .find_map(|(i, window)| if window == b"\r\n" { Some(i) } else { None })
}

fn fold_header(
    line: &str,
    line_length_limit: usize,
    skip: usize,
) -> Result<(&str, &str), Error> {
    // No need to fold if the line fits within the limit.
    if line.len() <= line_length_limit {
        Ok((line, ""))
    } else {
        // Otherwise look for where to split the line,
        // starting at the limit and searching back.
        line.char_indices()
            .rev()
            .skip_while(|(i, _)| *i > line_length_limit)
            .take_while(|(i, _)| *i >= skip)
            .find_map(|(i, c)| {
                WSP.find(c).map(|_| i)
            })
            .map_or_else(
                || {
                    // Error if we couldn't find a place to split.
                    Err(Error::HeaderLineCouldNotBeFolded(
                        String::from(line)
                    ))
                },
                |i| {
                    // The first part leads up to the split point.
                    let part = &line[..i];

                    // Find the last whitespace character
                    // starting at the split point.  This keeps one
                    // whitespace character and drops the rest.
                    let j = i + line[i..].char_indices()
                        .take_while(|(_, c)| WSP.find(*c).is_some())
                        .last()
                        .map(|(i, _)| i)
                        .unwrap();
                    Ok((part, &line[j..]))
                }
            )
    }
}

fn separate_header_name_and_value(line: &str) -> Result<Header, Error> {
    match line.find(':') {
        None => Err(Error::HeaderLineMissingColon(line.to_string())),
        Some(name_value_delimiter) => {
            let name = &line[0..name_value_delimiter];
            validate_header_name(name)?;
            Ok(Header{
                name: name.into(),
                value: line[name_value_delimiter+1..].to_string()
            })
        },
    }
}

fn unfold_header(
    mut raw_message: &[u8],
    mut header: Header,
) -> Result<Option<(Header, usize)>, Error> {
    let mut consumed = 0;
    loop {
        // Find where the next line ends.
        let line_terminator = match find_crlf(raw_message) {
            None => return Ok(None),
            Some(i) => i
        };

        // Calculate the next line's length, and convert it into a string.
        let line_length = line_terminator + 2;
        let line = std::str::from_utf8(&raw_message[..line_terminator])
            .map_err(|error| Error::HeaderLineInvalidText{
                line: raw_message[..line_terminator].to_vec(),
                source: error,
            })?;

        // If the next line begins with whitespace, unfold the line
        if
            line_terminator > 0  // line is not empty
            && WSP.find(         // and we can find whitespace
                line             // in the message
                    .chars()
                    .next()      // beginning at the line start
                    .unwrap()
            ).is_some()
        {
            // Append a single space to the header value.
            header.value.push(' ');

            // Concatenate the next line to the header value after trimming it.
            validate_header_value(&header, line)?;
            header.value += line.trim();

            // Move to the line following the next line.
            raw_message = &raw_message[line_length..];
            consumed += line_length;
        } else {
            return Ok(Some((header, consumed)));
        }
    }
}

fn validate_header_name(text: &str) -> Result<(), Error> {
    if text.chars()
        .all(|c| c.is_ascii_graphic())
    {
        Ok(())
    } else {
        Err(Error::HeaderNameContainsIllegalCharacter(text.to_string()))
    }
}

fn validate_header_value(
    header: &Header,
    value_segment: &str
) -> Result<(), Error> {
    if value_segment.chars()
        .all(|c|
            c == '\t'
            || c == ' '
            || (c as char).is_ascii_graphic()
        )
    {
        Ok(())
    } else {
        Err(Error::HeaderValueContainsIllegalCharacter{
            name: header.name.clone(),
            value_segment: value_segment.to_string()
        })
    }
}

/// This is a newtype wrapping a `String` in order to handle it
/// as an Internet message header name.
///
/// Names of headers in Internet messages are case insensitive.
/// This type is used to represent those names in such a way that
/// case insensitivity is considered for equality and order.
#[derive(Clone, Debug, Default, Eq)]
pub struct HeaderName(String);

impl From<&str> for HeaderName {
    fn from(name: &str) -> Self {
        Self(name.to_string())
    }
}

impl AsRef<str> for HeaderName {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl std::fmt::Display for HeaderName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_ref())
    }
}

impl PartialEq for HeaderName {
    fn eq(&self, rhs: &Self) -> bool {
        self.0.eq_ignore_ascii_case(&rhs.0)
    }
}

impl PartialEq<&str> for HeaderName {
    fn eq(&self, rhs: &&str) -> bool {
        self.0.eq_ignore_ascii_case(*rhs)
    }
}

impl PartialEq<HeaderName> for &str {
    fn eq(&self, rhs: &HeaderName) -> bool {
        self.eq_ignore_ascii_case(&rhs.0)
    }
}

impl PartialOrd for HeaderName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(
            match self.0.chars()
                .zip(other.0.chars())
                .find_map(|(lhs, rhs)| {
                    match lhs.to_ascii_lowercase()
                        .cmp(&rhs.to_ascii_lowercase())
                    {
                        std::cmp::Ordering::Equal => None,
                        ordering => Some(ordering),
                    }
                })
            {
                Some(ordering) => ordering,
                None => {
                    self.0.len()
                        .cmp(&other.0.len())
                },
            }
        )
    }
}

/// This represents one line of the headers portion of an Internet message.  It
/// consists of a case-insensitive name and a value, which may contain parts
/// separated by commas.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Header {
    /// This is the name of the header.  It is case-insensitive.
    pub name: HeaderName,

    /// This is the value of the header.  When parsed from text, or in text
    /// generated from it, this string may span (or fold across) multiple
    /// lines.  Also, in some contexts, it may contain multiple values,
    /// in which case the values are commonly separated by commas.
    pub value: String,
}

/// This enumerates the possible non-error states `MessageHeaders` can be in
/// after parsing a bit of input.
#[derive(Debug, Eq, PartialEq)]
pub enum ParseStatus {
    /// The message headers were fully parsed and the body (if any) begins
    /// after the last byte consumed.
    Complete,

    /// The message headers have not yet been fully parsed.
    ///
    /// The user is expected to call `parse` again with more input, starting
    /// with the unparsed portion of the previous input string, and adding more
    /// to it.
    Incomplete,
}

/// This holds the values returned by `MessageHeaders::parse`.
#[derive(Debug, Eq, PartialEq)]
pub struct ParseResults {
    /// This indicates the state of the parser.
    pub status: ParseStatus,

    /// This is the number of bytes of input consumed by the last
    /// `MessageHeaders::parse` call.
    pub consumed: usize,
}

/// This enumerates the ways in which a multi-value header may be represented
/// in the generated text.
#[derive(Debug, Copy, Clone)]
pub enum HeaderMultiMode {
    /// Put all values in the same header line (possibly folded) and separate
    /// the values with commas.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # extern crate rhymessage;
    /// use rhymessage::{MessageHeaders, HeaderMultiMode};
    ///
    /// # fn main() -> Result<(), rhymessage::Error> {
    /// let via: Vec<String> = [
    ///     "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
    ///     "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
    ///     "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
    /// ].iter().map(|s| String::from(*s)).collect();
    /// let mut headers = MessageHeaders::new();
    /// headers.set_header_multi_value("Via", via.clone(), HeaderMultiMode::OneLine);
    /// assert_eq!(
    ///     Ok(concat!(
    ///         "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
    ///             "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
    ///             "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
    ///         "\r\n",
    ///     ).as_bytes()),
    ///     headers.generate().as_deref()
    /// );
    /// # Ok(())
    /// # }
    /// ```
    OneLine,

    /// Put each value in a separate header line (each possibly folded)
    /// where all the header lines have the same header name.  Note that
    /// the higher-layer protocol may have restrictions on what kinds of
    /// headers may express multiple values in this way.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # extern crate rhymessage;
    /// use rhymessage::{MessageHeaders, HeaderMultiMode};
    ///
    /// # fn main() -> Result<(), rhymessage::Error> {
    /// let via: Vec<String> = [
    ///     "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
    ///     "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
    ///     "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
    /// ].iter().map(|s| String::from(*s)).collect();
    /// let mut headers = MessageHeaders::new();
    /// headers.set_header_multi_value("Via", via.clone(), HeaderMultiMode::MultiLine);
    /// assert_eq!(
    ///     concat!(
    ///         "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n",
    ///         "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n",
    ///         "Via: SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
    ///         "\r\n",
    ///     ).as_bytes(),
    ///     headers.generate()?
    /// );
    /// # Ok(())
    /// # }
    MultiLine,
}

/// This type is used to parse and generate the headers of Internet messages,
/// which consist of text lines, with an empty line separating the headers from
/// the body.  The format is specified in [IETF RFC
/// 5322](https://tools.ietf.org/html/rfc5322).
#[derive(Clone, Debug, Default)]
pub struct MessageHeaders {
    headers: Vec<Header>,
    line_length_limit: Option<usize>,
}

impl MessageHeaders {
    /// Append the given header.  This does not change or remove any previously
    /// added headers, even if they have the same name.  To replace a
    /// previously added header, use [`set_header`](#method.set_header)
    /// instead.
    pub fn add_header<H>(&mut self, header: H)
        where H: Into<Header>
    {
        self.headers.push(header.into());
    }

    /// Append one or more headers with the given name and values derived from
    /// the given vector, depending on the given mode (see
    /// [`HeaderMultiMode`](enum.HeaderMultiMode.html) for more information).
    /// This does not change or remove any previously added headers, even if
    /// they have the same name.  To replace a previously added header, use
    /// [`set_header_multi_value`](#method.set_header_multi_value) instead.
    pub fn add_header_multi_value<N, V>(
        &mut self,
        name: N,
        values: V,
        mode: HeaderMultiMode
    )
        where N: AsRef<str>, V: Into<Vec<String>>
    {
        let values = values.into();
        if values.is_empty() {
            return;
        }
        match mode {
            HeaderMultiMode::OneLine => {
                self.add_header(Header{
                    name: name.as_ref().into(),
                    value: values.join(",")
                });
            },
            HeaderMultiMode::MultiLine => {
                for value in values {
                    let name = name.as_ref();
                    self.add_header(Header{
                        name: name.into(),
                        value
                    });
                }
            },
        }
    }

    /// Produce the text string form of the message, according to
    /// the rules of RFC 5322:
    ///
    /// * Each header is separated by a carriage-return line-feed
    ///   sequence (CRLF).
    /// * Headers may be folded onto multiple lines at whitespace
    ///   characters (which are strictly space ('\x20') and horizontal
    ///   tab ('\x09').
    /// * The first line of a header consists of the header name
    ///   followed by a colon (':') followed by a space ('\x20')
    ///   followed by part or all of the header value.
    /// * After all headers, an empty line is placed to mark where
    ///   the body begins.
    ///
    /// # Errors
    ///
    /// `Error::HeaderLineCouldNotBeFolded` is returned if the generator
    /// is configured with a line limit constraint and one or more headers
    /// are too long and cannot be folded to fit within the constraint.
    ///
    /// While technically it shouldn't happen, logically we may return
    /// `Error::StringFormat` if one of the internal string formatting
    /// functions should fail (which they shouldn't unless something
    /// used internally doesn't implement `Display` properly.
    pub fn generate(&self) -> Result<Vec<u8>, Error> {
        let mut raw_string = String::new();
        for header in &self.headers {
            let line_buffer = format!("{}: {}", header.name.0, header.value);
            if let Some(line_length_limit) = self.line_length_limit {
                let mut rest = &line_buffer[..];
                let mut skip = header.name.0.len() + 2;
                while !rest.is_empty() {
                    let (part, rest_out) = fold_header(
                        rest,
                        line_length_limit - 2,
                        skip
                    )?;
                    rest = rest_out;
                    skip = 1;
                    write!(&mut raw_string, "{}{}", part, CRLF)?;
                }
            } else {
                write!(&mut raw_string, "{}{}", line_buffer, CRLF)?;
            }
        }
        write!(&mut raw_string, "{}", CRLF)?;
        Ok(raw_string.into_bytes())
    }

    /// Borrow the list of headers.
    #[must_use]
    pub fn headers(&self) -> &Vec<Header> {
        &self.headers
    }

    /// Produce a list of the values for the headers with the given name.
    /// If no headers have the given name, an empty list is returned.
    #[must_use]
    pub fn header_multi_value<T>(&self, name: T) -> Vec<String>
        where T: AsRef<str>
    {
        let name = name.as_ref();
        self.headers.iter()
            .filter_map(|header| {
                if header.name == name {
                    Some(header.value.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Take the value of the header with the given name, interpret it as a
    /// comma-separated list of values, and produce that list.  If no headers
    /// have the given name, an empty list is returned.
    #[must_use]
    pub fn header_tokens<T>(&self, name: T) -> Vec<String>
        where T: AsRef<str>
    {
        self.header_multi_value(name).iter()
            .flat_map(|value| {
                value.split_terminator(',')
                    .map(str::trim)
                    .map(str::to_ascii_lowercase)
            })
            .collect()
    }

    /// Produce the value for the header with the given name, if any.
    /// If there are two or more headers with the same name, the produced
    /// string will be the joining of their values with commas.
    #[must_use]
    pub fn header_value<T>(&self, name: T) -> Option<String>
        where T: AsRef<str>
    {
        let name = name.as_ref();
        self.headers.iter().fold(None, |composite, header| {
            if header.name == name {
                Some(
                    composite.map_or_else(
                        || header.value.clone(),
                        |mut composite| {
                            composite += ",";
                            composite += &header.value;
                            composite
                        })
                )
            } else {
                composite
            }
        })
    }

    /// This is a convenience function which essentially looks up the tokens
    /// in a header using [`header_tokens`](#method.header_tokens)
    /// and searches them to see if the given token is among them.
    pub fn has_header_token<T>(
        &self,
        name: T,
        token: T
    ) -> bool
        where T: AsRef<str>
    {
        let token = token.as_ref();
        let normalized_token = token.to_ascii_lowercase();
        self.header_tokens(name)
            .iter()
            .any(|token_in_header| *token_in_header == normalized_token)
    }

    /// Determine whether or not the given header is present.
    #[must_use]
    pub fn has_header<T>(&self, name: T) -> bool
        where T: AsRef<str>
    {
        let name = name.as_ref();
        self.headers.iter().any(|header| {
            header.name == name
        })
    }

    /// Create a new instance with no headers in it.
    #[must_use]
    pub fn new() -> Self {
        Self{
            headers: Vec::new(),
            line_length_limit: None,
        }
    }

    /// Feed more Internet message text into the parser, building the
    /// collection of headers internally, and detecting when the end of
    /// the headers and the beginning of the body is found.
    ///
    /// This function may be called multiple times to parse input
    /// incrementally.  Each call returns an indication of whether or
    /// not a message was parsed and how many input bytes were consumed.
    ///
    /// # Errors
    ///
    /// * `HeaderLineTooLong` &nash; the parser is configured
    ///   with a line length constraint and the input exceeds it on a line
    /// * `HeaderLineMissingColon` &ndash; there is no colon between
    ///   the name and value of a header line
    /// * `HeaderNameContainsIllegalCharacter` &ndash; the name of a
    ///   header contains an illegal character
    /// * `HeaderValueContainsIllegalCharacter` &ndash; the value of a
    ///   header contains an illegal character
    ///
    /// # Examples
    ///
    /// ```rust
    /// # extern crate rhymessage;
    /// use rhymessage::{MessageHeaders, ParseResults, ParseStatus};
    ///
    /// # fn main() -> Result<(), rhymessage::Error> {
    /// let mut headers = MessageHeaders::new();
    ///
    /// // `parse` does not consume first line because the next line
    /// // it hasn't seen yet might contain part of the "From" header.
    /// assert_eq!(
    ///     ParseResults{
    ///         status: ParseStatus::Incomplete,
    ///         consumed: 0,
    ///     },
    ///     headers.parse("From: joe@example.com\r\n")?
    /// );
    ///
    /// // So we re-send the "From" line along with the content that follows.
    /// // At this point, `parse` knows it has the full "From" header, and
    /// // consumes it, but not the "To" header, since the next line might
    /// // be a continuation of it.
    /// assert_eq!(
    ///     ParseResults{
    ///         status: ParseStatus::Incomplete,
    ///         consumed: 23,
    ///     },
    ///     headers.parse(concat!(
    ///         "From: joe@example.com\r\n",
    ///         "To: sal@example.com\r\n",
    ///     ))?
    /// );
    ///
    /// // So we re-send the "To" line along with the content that follows.
    /// // At this point, `parse` knows it has the full "To" header, and
    /// // consumes it, but not the "Subject" header, since the next line might
    /// // be a continuation of it.
    /// assert_eq!(
    ///     ParseResults{
    ///         status: ParseStatus::Incomplete,
    ///         consumed: 21,
    ///     },
    ///     headers.parse(concat!(
    ///         "To: sal@example.com\r\n",
    ///         "Subject: Hello,\r\n",
    ///     ))?
    /// );
    ///
    /// // So we re-send the "Subject" line along with the content that
    /// // follows.  At this point, `parse` still doesn't know it has the
    /// // full "Subject" header, so nothing is consumed.
    /// assert_eq!(
    ///     ParseResults{
    ///         status: ParseStatus::Incomplete,
    ///         consumed: 0,
    ///     },
    ///     headers.parse(concat!(
    ///         "Subject: Hello,\r\n",
    ///         " World!\r\n",
    ///     ))?
    /// );
    ///
    /// // So we re-send again with even more content.  At this point,
    /// // `parse` sees the empty line separating headers from body,
    /// // so it can finally consume the "Subject" header as well as
    /// // indicate that parsing the headers is complete.
    /// assert_eq!(
    ///     ParseResults{
    ///         status: ParseStatus::Complete,
    ///         consumed: 28,
    ///     },
    ///     headers.parse(concat!(
    ///         "Subject: Hello,\r\n",
    ///         " World!\r\n",
    ///         "\r\n",
    ///     ))?
    /// );
    /// # Ok(())
    /// # }
    pub fn parse<T>(
        &mut self,
        raw_message: T
    ) -> Result<ParseResults, Error>
        where T: AsRef<[u8]>
    {
        let mut offset = 0;
        let raw_message = raw_message.as_ref();
        while offset < raw_message.len() {
            // Find the end of the current line.
            let line_terminator = find_crlf(&raw_message[offset..]);
            if line_terminator.is_none() {
                if let Some(line_length_limit) = self.line_length_limit {
                    let unterminated_line_length = raw_message.len() - offset;
                    if unterminated_line_length + 2 > line_length_limit {
                        let end = std::cmp::min(
                            unterminated_line_length,
                            line_length_limit
                        );
                        return Err(Error::HeaderLineTooLong(
                            raw_message[offset..offset+end].to_vec()
                        ));
                    }
                }
                break;
            }

            // Bail if the line is longer than the limit (if set).
            let line_terminator = line_terminator.unwrap();
            if let Some(line_length_limit) = self.line_length_limit {
                if line_terminator + CRLF.len() > line_length_limit {
                    return Err(Error::HeaderLineTooLong(
                        raw_message[offset..offset+line_length_limit].to_vec()
                    ));
                }
            }

            // Stop if empty line is found -- this is where
            // the headers end and the body (which we don't parse,
            // but leave up to the user to handle) begins.
            if line_terminator == 0 {
                offset += CRLF.len();
                return Ok(ParseResults{
                    status: ParseStatus::Complete,
                    consumed: offset,
                });
            }

            // Separate the header name from the header value.
            let header = separate_header_name_and_value(
                std::str::from_utf8(&raw_message[offset..offset+line_terminator])
                    .map_err(|error| Error::HeaderLineInvalidText{
                        line: raw_message[offset..offset+line_terminator].to_vec(),
                        source: error
                    })?
            )?;
            // Check the first value segment for validity.
            validate_header_value(&header, &header.value)?;

            // Look ahead in the raw message and perform line unfolding
            // if we see any lines that begin with whitespace.
            let next_offset = offset + line_terminator + CRLF.len();
            if let Some((mut header, consumed)) = unfold_header(
                &raw_message[next_offset..],
                header
            )? {
                // Remove any whitespace that might be at the beginning
                // or end of the header value, and then store the
                // header.
                header.value = header.value.trim().to_string();
                self.headers.push(header);
                offset = next_offset + consumed;
            } else {
                return Ok(ParseResults{
                    status: ParseStatus::Incomplete,
                    consumed: offset,
                });
            }
        }
        Ok(ParseResults{
            status: ParseStatus::Incomplete,
            consumed: offset,
        })
    }

    /// Remove all headers with the given name.
    pub fn remove_header<N>(&mut self, name: N)
        where N: AsRef<str>
    {
        let name = name.as_ref();
        self.headers.retain(|header| {
            header.name != name
        });
    }

    /// Add or replace a header with the given name and value.  If one or more
    /// previously added headers has the same name, the first one is changed to
    /// hold the given value, and the others are removed.  To add a new header
    /// without replacing or removing previously added ones with the same name,
    /// use [`add_header`](#method.add_header) instead.
    pub fn set_header<N, V>(&mut self, name: N, value: V)
        where N: AsRef<str>, V: Into<String>
    {
        let name = name.as_ref();
        let mut value = value.into();

        // This code is inspired by `Vec::retain`.  We're essentially
        // doing the same thing, except we're modifying the
        // first matching header rather than discarding it.
        let len = self.headers.len();
        let mut matches = 0;
        for i in 0..len {
            if self.headers[i].name == name {
                if matches == 0 {
                    std::mem::swap(
                        &mut self.headers[i].value,
                        &mut value
                    );
                }
                matches += 1;
            } else if matches > 1 {
                self.headers.swap(i - matches + 1, i);
            }
        }
        if matches > 1 {
            self.headers.truncate(len - matches + 1);
        } else if matches == 0 {
            self.add_header(Header{
                name: name.into(),
                value
            });
        }
    }

    /// Add or replace one or more headers with the given name and values
    /// derived from the given vector, depending on the given mode (see
    /// [`HeaderMultiMode`](enum.HeaderMultiMode.html) for more information).
    /// If one or more previously added headers has the same name, the first
    /// one is changed to hold the given value(s), and the others are removed.
    /// To add new headers without replacing or removing previously added ones
    /// with the same name, use
    /// [`add_header_multi_value`](#method.add_header_multi_value) instead.
    pub fn set_header_multi_value<N, V>(
        &mut self,
        name: N,
        values: V,
        mode: HeaderMultiMode
    )
        where N: AsRef<str>, V: Into<Vec<String>>
    {
        let values = values.into();
        if values.is_empty() {
            return;
        }
        match mode {
            HeaderMultiMode::OneLine => {
                self.set_header(name, values.join(","));
            },
            HeaderMultiMode::MultiLine => {
                let name = name.as_ref();
                for (i, value) in values.into_iter().enumerate() {
                    if i == 0 {
                        self.set_header(name, value);
                    } else {
                        self.add_header(Header{
                            name: name.into(),
                            value
                        });
                    }
                }
            },
        }
    }

    /// This configures the parser and generator to constrain the lengths
    /// of header lines to no more than the given number, if any.  Setting
    /// `None` removes any constraint.
    pub fn set_line_limit(&mut self, limit: Option<usize>) {
        self.line_length_limit = limit;
    }

}

impl IntoIterator for MessageHeaders {
    type Item = Header;
    type IntoIter = std::vec::IntoIter<Header>;

    fn into_iter(self) -> Self::IntoIter {
        self.headers.into_iter()
    }
}

impl<'a> IntoIterator for &'a MessageHeaders {
    type Item = &'a Header;
    type IntoIter = std::slice::Iter<'a, Header>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.headers).iter()
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn header_name_equivalency() {
        named_tuple!(
            struct TestVector {
                expected_result: bool,
                lhs: &'static str,
                rhs: &'static str,
            }
        );
        let test_vectors: &[TestVector] = &[
            (true, "hello", "hello").into(),
            (true, "Hello", "hello").into(),
            (false, "jello", "hello").into(),
            (false, "hello", "hell").into(),
        ];
        for test_vector in test_vectors.iter() {
            let lhs = HeaderName::from(*test_vector.lhs());
            let rhs = HeaderName::from(*test_vector.rhs());
            assert_eq!(
                *test_vector.expected_result(),
                (lhs == rhs)
            );
        }
    }

    #[test]
    fn header_name_rank() {
        named_tuple!(
            struct TestVector {
                expected_result: bool,
                lhs: &'static str,
                rhs: &'static str,
            }
        );
        let test_vectors: &[TestVector] = &[
            (false, "hello", "hello").into(),
            (false, "Hello", "hello").into(),
            (false, "hello", "Hello").into(),
            (false, "jello", "hello").into(),
            (true, "hello", "jello").into(),
            (false, "hello", "hell").into(),
            (true, "hell", "hello").into(),
        ];
        for test_vector in test_vectors.iter() {
            let lhs = HeaderName::from(*test_vector.lhs());
            let rhs = HeaderName::from(*test_vector.rhs());
            assert_eq!(
                *test_vector.expected_result(),
                (lhs < rhs),
                "{} < {}", test_vector.lhs(), test_vector.rhs()
            );
        }
    }

    #[test]
    fn header_name_to_string() {
        let name = HeaderName::from("Content-Length");
        let name_as_str = name.as_ref();
        assert_eq!("Content-Length", name);
        assert_eq!("Content-Length", name_as_str);
        assert_eq!("Content-Length", name.to_string());
        assert_eq!("Content-Length", format!("{}", name));
    }

    #[test]
    fn http_client_request_message() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        let header_collection = headers.headers();
        named_tuple!(
            struct ExpectedHeader {
                name: &'static str,
                value: &'static str,
            }
        );
        let expected_headers: &[ExpectedHeader] = &[
            ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3").into(),
            ("Host", "www.example.com").into(),
            ("Accept-Language", "en, mi").into(),
        ];
        assert_eq!(expected_headers.len(), header_collection.len());
        expected_headers.iter()
            .zip(header_collection.iter())
            .for_each(|(expected, actual)| {
                assert_eq!(*expected.name(), actual.name);
                assert_eq!(*expected.value(), actual.value);
            });
        assert!(headers.has_header("Host"));
        assert!(!headers.has_header("Foobar"));
        assert_eq!(
            Ok(raw_message.as_bytes()),
            headers.generate().as_deref()
        );
    }

    #[test]
    fn http_server_response_message() {
        let mut headers = MessageHeaders::new();
        let raw_headers = concat!(
            "Date: Mon, 27 Jul 2009 12:28:53 GMT\r\n",
            "Server: Apache\r\n",
            "Last-Modified: Wed, 22 Jul 2009 19:15:56 GMT\r\n",
            "ETag: \"34aa387-d-1568eb00\"\r\n",
            "Accept-Ranges: bytes\r\n",
            "Content-Length: 51\r\n",
            "Vary: Accept-Encoding\r\n",
            "Content-Type: text/plain\r\n",
            "\r\n",
        );
        let raw_message = String::from(raw_headers)
            + "Hello World! My payload includes a trailing CRLF.\r\n";
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_headers.len(),
            }),
            headers.parse(raw_message)
        );
        let header_collection = headers.headers();
        named_tuple!(
            struct ExpectedHeader {
                name: &'static str,
                value: &'static str,
            }
        );
        let expected_headers: &[ExpectedHeader] = &[
            ("Date", "Mon, 27 Jul 2009 12:28:53 GMT").into(),
            ("Server", "Apache").into(),
            ("Last-Modified", "Wed, 22 Jul 2009 19:15:56 GMT").into(),
            ("ETag", "\"34aa387-d-1568eb00\"").into(),
            ("Accept-Ranges", "bytes").into(),
            ("Content-Length", "51").into(),
            ("Vary", "Accept-Encoding").into(),
            ("Content-Type", "text/plain").into(),
        ];
        assert_eq!(expected_headers.len(), header_collection.len());
        expected_headers.iter()
            .zip(header_collection.iter())
            .for_each(|(expected, actual)| {
                assert_eq!(*expected.name(), actual.name);
                assert_eq!(*expected.value(), actual.value);
            });
        assert!(headers.has_header("Last-Modified"));
        assert!(!headers.has_header("Foobar"));
        assert_eq!(
            Ok(raw_headers.as_bytes()),
            headers.generate().as_deref()
        );
    }

    #[test]
    fn header_line_almost_too_long() {
        let mut headers = MessageHeaders::new();
        headers.set_line_limit(Some(1000));
        let test_header_name = "X-Poggers";
        let test_header_name_with_delimiters = String::from(test_header_name) + ": ";
        let longest_possible_poggers = "X".repeat(
            998 - test_header_name_with_delimiters.len()
        );
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
        ).to_string()
            + &test_header_name_with_delimiters + &longest_possible_poggers + "\r\n"
            + "Accept-Language: en, mi\r\n"
            + "\r\n";
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        assert_eq!(
            Some(longest_possible_poggers),
            headers.header_value(test_header_name)
        );
    }

    #[test]
    fn header_line_too_long() {
        let mut headers = MessageHeaders::new();
        headers.set_line_limit(Some(1000));
        let test_header_name = "X-Poggers";
        let test_header_name_with_delimiters = String::from(test_header_name) + ": ";
        let too_long_poggers = "X".repeat(
            999 - test_header_name_with_delimiters.len()
        );
        let too_long_header = test_header_name_with_delimiters.clone() + &too_long_poggers + "\r\n";
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
        ).to_string()
            + &test_header_name_with_delimiters + &too_long_poggers + "\r\n"
            + "Accept-Language: en, mi\r\n"
            + "\r\n";
        assert_eq!(
            Err(Error::HeaderLineTooLong(too_long_header[0..1000].as_bytes().to_vec())),
            headers.parse(raw_message)
        );
    }

    #[test]
    fn header_line_too_long_and_not_terminated() {
        let mut headers = MessageHeaders::new();
        headers.set_line_limit(Some(1000));
        let test_header_name = "X-Poggers";
        let test_header_name_with_delimiters = String::from(test_header_name) + ": ";
        let value_is_too_long = "X".repeat(
            999 - test_header_name_with_delimiters.len()
        );
        let raw_message = test_header_name_with_delimiters + &value_is_too_long;
        assert_eq!(
            Err(Error::HeaderLineTooLong(raw_message.clone().into_bytes())),
            headers.parse(raw_message)
        );
    }

    #[test]
    fn header_line_over1000_characters_allowed_by_default() {
        let mut headers = MessageHeaders::new();
        let test_header_name = "X-Poggers";
        let test_header_name_with_delimiters = String::from(test_header_name) + ": ";
        let long_poggers = "X".repeat(
            999 - test_header_name_with_delimiters.len()
        );
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
        ).to_string()
            + &test_header_name_with_delimiters + &long_poggers + "\r\n"
            + "Accept-Language: en, mi\r\n"
            + "\r\n";
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        assert_eq!(
            Some(long_poggers),
            headers.header_value(test_header_name)
        );
    }

    #[test]
    fn empty_message() {
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Incomplete,
                consumed: 0,
            }),
            headers.parse("")
        );
    }

    #[test]
    fn single_line_truncated() {
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Incomplete,
                consumed: 0,
            }),
            headers.parse("User-Agent: curl")
        );
    }

    #[test]
    fn single_line_not_truncated() {
        let mut headers = MessageHeaders::new();
        let input = "User-Agent: curl\r\n\r\n";
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: input.len(),
            }),
            headers.parse(input)
        );
    }

    #[test]
    fn no_headers_at_all() {
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: 2,
            }),
            headers.parse("\r\n Something Else Not Part Of The Message")
        );
        assert!(headers.headers().is_empty());
    }

    #[test]
    fn get_value_of_present_header() {
        let mut headers = MessageHeaders::new();
        let raw_message_headers = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        let raw_message = String::from(raw_message_headers)
            + " Something Else Not Part Of The Message";
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message_headers.len(),
            }),
            headers.parse(raw_message)
        );
        assert_eq!(
            Some("www.example.com"),
            headers.header_value("Host").as_deref()
        );
    }

    #[test]
    fn set_header_add() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        headers.set_header("X", "PogChamp");
        assert_eq!(
            Ok(concat!(
                "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
                "Host: www.example.com\r\n",
                "Accept-Language: en, mi\r\n",
                "X: PogChamp\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
    }

    #[test]
    fn set_header_replace() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        headers.set_header("Host", "example.com");
        assert_eq!(
            Ok(concat!(
                "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
                "Host: example.com\r\n",
                "Accept-Language: en, mi\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
    }

    #[test]
    fn get_value_of_missing_header() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        assert_eq!(
            None,
            headers.header_value("PePe")
        );
    }

    #[test]
    fn header_with_character_less_than_33_in_name() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Feels Bad Man: LUL\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        assert_eq!(
            Err(Error::HeaderNameContainsIllegalCharacter(
                String::from("Feels Bad Man")
            )),
            headers.parse(raw_message)
        );
    }

    #[test]
    fn header_with_character_greater_than_126_in_name() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "FeelsBadMan\x7f: LUL\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        assert_eq!(
            Err(Error::HeaderNameContainsIllegalCharacter(
                String::from("FeelsBadMan\x7f")
            )),
            headers.parse(raw_message)
        );
    }

    #[test]
    fn header_with_illegal_character_in_value_first_segment() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.çom\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        assert_eq!(
            Err(Error::HeaderValueContainsIllegalCharacter{
                name: HeaderName::from("Host"),
                value_segment: String::from(" www.example.çom")
            }),
            headers.parse(raw_message)
        );
    }

    #[test]
    fn header_with_illegal_character_in_value_second_segment() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en,\r\n",
            " mi, ça\r\n",
            "\r\n",
        );
        assert_eq!(
            Err(Error::HeaderValueContainsIllegalCharacter{
                name: HeaderName::from("Accept-Language"),
                value_segment: String::from(" mi, ça")
            }),
            headers.parse(raw_message)
        );
    }

    #[test]
    fn header_with_colon_in_name() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Feels:BadMan: LUL\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        let header_collection = headers.headers();
        named_tuple!(
            struct ExpectedHeader {
                name: &'static str,
                value: &'static str,
            }
        );
        let expected_headers: &[ExpectedHeader] = &[
            ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3").into(),
            ("Host", "www.example.com").into(),
            ("Feels", "BadMan: LUL").into(),
            ("Accept-Language", "en, mi").into(),
        ];
        assert_eq!(expected_headers.len(), header_collection.len());
        expected_headers.iter()
            .zip(header_collection.iter())
            .for_each(|(expected_header, actual_header)| {
                assert_eq!(*expected_header.name(), actual_header.name);
                assert_eq!(*expected_header.value(), actual_header.value);
            });
    }

    #[test]
    fn header_value_unfolding_single_wsp() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "Subject: This\r\n",
            " is a test\r\n",
            "\r\n",
        );
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        assert_eq!(
            Some("This is a test"),
            headers.header_value("Subject").as_deref()
        );
    }

    #[test]
    fn header_value_unfolding_multiple_wsp() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "Subject: This\r\n",
            "    is a test\r\n",
            "\r\n",
        );
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        assert_eq!(
            Some("This is a test"),
            headers.header_value("Subject").as_deref()
        );
    }

    #[test]
    fn fold_line_that_would_exceed_limit() {
        let header_name = "X";
        named_tuple!(
            struct TestVector {
                header_value: &'static str,
                expected_lines: Option<&'static [&'static str]>,
            }
        );
        let test_vectors: &[TestVector] = &[
            // ...................... Some(&["..........", "..........", "..........", "", ""][..])).into(),
            ("Hello!",                Some(&["X: Hello!" ,                             "", ""][..])).into(),
            ("Hello!!",               Some(&["X: Hello!!",                             "", ""][..])).into(),
            ("Hello!!!",              None                                                         ).into(),
            ("Hello, World!",         Some(&["X: Hello," , " World!"   ,               "", ""][..])).into(),
            ("This is even longer!",  Some(&["X: This is", " even"     , " longer!"  , "", ""][..])).into(),
            ("This is even long er!", Some(&["X: This is", " even long", " er!"      , "", ""][..])).into(),
            ("This is evenlonger!",   None                                                         ).into(),
            ("sadfjkasdfjlkasdfjla",  None                                                         ).into(),
        ];
        for test_vector in test_vectors {
            let mut headers = MessageHeaders::new();
            headers.set_line_limit(Some(12));
            headers.set_header(header_name, *test_vector.header_value());
            let raw_headers = headers.generate();
            if let Some(expected_lines) = test_vector.expected_lines() {
                assert!(raw_headers.is_ok());
                let raw_headers = raw_headers.unwrap();
                let raw_headers = std::str::from_utf8(&raw_headers).unwrap();
                let actual_lines = raw_headers.split("\r\n").collect::<Vec<&str>>();
                assert_eq!(*expected_lines, actual_lines);
            } else {
                assert!(raw_headers.is_err());
            }
        }
    }

    #[test]
    fn header_names_should_be_case_insensive() {
        named_tuple!(
            struct TestVector {
                header_name: &'static str,
                should_also_match: &'static [&'static str],
            }
        );
        let test_vectors: &[TestVector] = &[
            ("Content-Type", &["content-type", "CONTENT-TYPE", "Content-type", "CoNtENt-TYpe"][..]).into(),
            ("ETag", &["etag", "ETAG", "Etag", "eTag", "etaG"][..]).into(),
        ];
        for test_vector in test_vectors {
            let mut headers = MessageHeaders::new();
            headers.set_header(test_vector.header_name(), "HeyGuys");
            for alternative in *test_vector.should_also_match() {
                assert!(headers.has_header(alternative));
            }
        }
    }

    #[test]
    fn get_header_multiple_values() {
        let raw_message = concat!(
            "Via: SIP/2.0/UDP server10.biloxi.com\r\n",
            "    ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n",
            "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com\r\n",
            "    ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n",
            "Via: SIP/2.0/UDP pc33.atlanta.com\r\n",
            "    ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
            "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
            "\r\n",
        );
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        assert_eq!(
            Some(concat!(
                "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1",
            )),
            headers.header_value("Via").as_deref()
        );
        assert_eq!(
            &[
                "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
                "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
                "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1",
            ][..],
            headers.header_multi_value("Via")
        );
        assert_eq!(
            Some("Bob <sip:bob@biloxi.com>;tag=a6c85cf"),
            headers.header_value("To").as_deref()
        );
        assert_eq!(
            &[
                "Bob <sip:bob@biloxi.com>;tag=a6c85cf"
            ][..],
            headers.header_multi_value("To")
        );
        assert!(
            headers.header_multi_value("PogChamp").is_empty()
        );
    }

    #[test]
    fn set_header_multi_valueple_values() {
        let via: Vec<String> = [
            "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
        ].iter().map(|s| String::from(*s)).collect();
        let to: Vec<String> = [
            "Bob <sip:bob@biloxi.com>;tag=a6c85cf"
        ].iter().map(|s| String::from(*s)).collect();
        let mut headers = MessageHeaders::new();
        headers.set_header_multi_value("Via", via.clone(), HeaderMultiMode::OneLine);
        headers.set_header_multi_value("To", to.clone(), HeaderMultiMode::OneLine);
        headers.set_header_multi_value("FeelsBadMan", vec![] as Vec<String>, HeaderMultiMode::OneLine);
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                    "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                    "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
        headers = MessageHeaders::new();
        headers.set_header_multi_value("Via", via, HeaderMultiMode::MultiLine);
        headers.set_header_multi_value("To", to, HeaderMultiMode::MultiLine);
        headers.set_header_multi_value("FeelsBadMan", vec![] as Vec<String>, HeaderMultiMode::MultiLine);
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n",
                "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n",
                "Via: SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
    }

    #[test]
    fn set_header_should_replace_all_previous_values() {
        let via: Vec<String> = [
            "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
        ].iter().map(|s| String::from(*s)).collect();
        let mut headers = MessageHeaders::new();
        headers.set_header_multi_value("Via", via, HeaderMultiMode::OneLine);
        headers.set_header("To", "Bob <sip:bob@biloxi.com>;tag=a6c85cf");
        headers.set_header("From", "Alice <sip:alice@atlanta.com>;tag=1928301774");
        headers.add_header(Header{
            name: "Via".into(),
            value: "Trickster".into()
        });
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                    "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                    "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "Via: Trickster\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
        headers.set_header("Via", "Kappa");
        assert_eq!(
            Ok(concat!(
                "Via: Kappa\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
    }

    #[test]
    fn add_header() {
        let via: Vec<String> = [
            "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
        ].iter().map(|s| String::from(*s)).collect();
        let mut headers = MessageHeaders::new();
        headers.set_header_multi_value("Via", via, HeaderMultiMode::OneLine);
        headers.set_header("To", "Bob <sip:bob@biloxi.com>;tag=a6c85cf");
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                    "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                    "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
        headers.add_header(Header{
            name: "From".into(),
            value: "Alice <sip:alice@atlanta.com>;tag=1928301774".into()
        });
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                    "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                    "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
        let x_pepe: Vec<String> = [
            "<3",
            "SeemsGood",
        ].iter().map(|s| String::from(*s)).collect();
        headers.add_header_multi_value("X-PePe", x_pepe, HeaderMultiMode::OneLine);
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                    "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                    "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "X-PePe: <3,SeemsGood\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
        let to: Vec<String> = ["Carol"].iter().map(|s| String::from(*s)).collect();
        headers.add_header_multi_value("To", to, HeaderMultiMode::OneLine);
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                    "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                    "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "X-PePe: <3,SeemsGood\r\n",
                "To: Carol\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
    }

    #[test]
    fn remove_header() {
        let via: Vec<String> = [
            "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
        ].iter().map(|s| String::from(*s)).collect();
        let mut headers = MessageHeaders::new();
        headers.set_header_multi_value("Via", via, HeaderMultiMode::MultiLine);
        headers.set_header("To", "Bob <sip:bob@biloxi.com>;tag=a6c85cf");
        headers.add_header(Header{
            name: "From".into(),
            value: "Alice <sip:alice@atlanta.com>;tag=1928301774".into()
        });
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n",
                "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n",
                "Via: SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
        headers.remove_header("From");
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n",
                "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n",
                "Via: SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
        headers.remove_header("Via");
        assert_eq!(
            Ok(concat!(
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "\r\n",
            ).as_bytes()),
            headers.generate().as_deref()
        );
    }

    #[test]
    fn header_tokens() {
        let raw_message = concat!(
            "Foo: bar, Spam,  heLLo\r\n",
            "Bar: Foo \r\n",
            "Spam:   \t  \r\n",
            "\r\n",
        );
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        assert_eq!(
            &[
                "bar",
                "spam",
                "hello",
            ][..],
            headers.header_tokens("Foo")
        );
        assert_eq!(
            &[
                "foo",
            ][..],
            headers.header_tokens("Bar")
        );
        assert_eq!(
            &[] as &[String],
            headers.header_tokens("Spam")
        );
    }

    #[test]
    fn has_header_token() {
        let raw_message = concat!(
            "Foo: bar, Spam,  heLLo\r\n",
            "Bar: Foo \r\n",
            "Spam:   \t  \r\n",
            "\r\n",
        );
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: raw_message.len(),
            }),
            headers.parse(raw_message)
        );
        assert!(headers.has_header_token("Foo", "bar"));
        assert!(headers.has_header_token("Foo", "Bar"));
        assert!(headers.has_header_token("Foo", "spam"));
        assert!(headers.has_header_token("Foo", "hello"));
        assert!(!headers.has_header_token("Foo", "xyz"));
        assert!(!headers.has_header_token("Foo", "secret_to_the_universe"));
        assert!(headers.has_header_token("Bar", "foo"));
        assert!(!headers.has_header_token("Bar", "spam"));
        assert!(!headers.has_header_token("Spam", "foo"));
        assert!(!headers.has_header_token("Spam", "spam"));
    }

    #[test]
    fn clone_headers() {
        let mut original_headers = MessageHeaders::new();
        original_headers.set_header("Foo", "Bar");
        original_headers.set_header("Hello", "World");
        let mut headers_copy = original_headers.clone();
        headers_copy.set_header("Hello", "PePe");
        assert_eq!(Some("Bar"), original_headers.header_value("Foo").as_deref());
        assert_eq!(Some("World"), original_headers.header_value("Hello").as_deref());
        assert_eq!(Some("Bar"), headers_copy.header_value("Foo").as_deref());
        assert_eq!(Some("PePe"), headers_copy.header_value("Hello").as_deref());
    }

    #[test]
    fn http_client_request_message_in_two_parts_divided_between_header_lines() {
        let mut headers = MessageHeaders::new();
        let raw_message_pieces = &[
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n"
        ][..];
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Incomplete,
                consumed: raw_message_pieces[0].len(),
            }),
            headers.parse(
                String::from(raw_message_pieces[0])
                + raw_message_pieces[1]
            )
        );
        assert_eq!(
            Ok(ParseResults{
                status: ParseStatus::Complete,
                consumed: (
                    raw_message_pieces[1].len()
                    + raw_message_pieces[2].len()
                    + raw_message_pieces[3].len()
                ),
            }),
            headers.parse(
                String::from(raw_message_pieces[1])
                + raw_message_pieces[2]
                + raw_message_pieces[3]
            )
        );
        let header_collection = headers.headers();
        named_tuple!(
            struct ExpectedHeader {
                name: &'static str,
                value: &'static str,
            }
        );
        let expected_headers: &[ExpectedHeader] = &[
            ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3").into(),
            ("Host", "www.example.com").into(),
            ("Accept-Language", "en, mi").into(),
        ];
        assert_eq!(expected_headers.len(), header_collection.len());
        expected_headers.iter()
            .zip(header_collection.iter())
            .for_each(|(expected_header, actual_header)| {
                assert_eq!(*expected_header.name(), actual_header.name);
                assert_eq!(*expected_header.value(), actual_header.value);
            });
        assert!(headers.has_header("Host"));
        assert!(!headers.has_header("Foobar"));
        assert_eq!(
            Ok(
                raw_message_pieces.iter()
                    .flat_map(|s| s.as_bytes())
                    .copied()
                    .collect()
            ),
            headers.generate()
        );
    }

    #[test]
    fn headers_into_iter_move() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        headers.parse(raw_message).unwrap();
        assert_eq!(
            vec![
                Header{
                    name: "User-Agent".into(),
                    value: "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3".into(),
                },
                Header{
                    name: "Host".into(),
                    value: "www.example.com".into(),
                },
                Header{
                    name: "Accept-Language".into(),
                    value: "en, mi".into(),
                },
            ],
            headers.into_iter().collect::<Vec<Header>>()
        );
    }

    #[test]
    fn headers_into_iter_borrow() {
        let mut headers = MessageHeaders::new();
        let raw_message = concat!(
            "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
            "Host: www.example.com\r\n",
            "Accept-Language: en, mi\r\n",
            "\r\n",
        );
        headers.parse(raw_message).unwrap();
        assert!(
            [
                Header{
                    name: "User-Agent".into(),
                    value: "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3".into(),
                },
                Header{
                    name: "Host".into(),
                    value: "www.example.com".into(),
                },
                Header{
                    name: "Accept-Language".into(),
                    value: "en, mi".into(),
                },
            ].iter().eq(
                (&headers).into_iter()
            )
        );
    }

}
