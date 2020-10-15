#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]

#[cfg(test)]
#[macro_use]
extern crate named_tuple;

mod error;

use error::Error;

// These are the characters that are considered whitespace and
// should be stripped off by the Strip() function.
//
// Name of "WSP" chosen to match the symbol name from
// RFC 5322 (https://tools.ietf.org/html/rfc5322) which refers
// to this specific character set.
const WSP: &str = " \t";

// This is the required line terminator for internet message header lines.
const CRLF: &str = "\r\n";

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

fn fold_header(
    line: &mut &str,
    line_length_limit: usize
) -> Result<String, Error> {
    todo!()
}

fn unfold_header(
    mut raw_message: &str,
    mut value: String,
) -> Result<Option<(String, usize)>, Error> {
    let mut consumed = 0;
    loop {
        // Find where the next line ends.
        let line_terminator = match raw_message.find(CRLF) {
            None => return Ok(None),
            Some(i) => i
        };

        // Calculate the next line's length.
        let line_length = line_terminator + CRLF.len();

        // If the next line begins with whitespace, unfold the line
        if
            line_terminator > 0  // line is not empty
            && WSP.find(         // and we can find whitespace
                raw_message      // in the message
                    .chars()
                    .next()      // beginning at the line start
                    .unwrap()
            ).is_some()
        {
            // Append a single space to the header value.
            value.push(' ');

            // Concatenate the next line to the header value after trimming it.
            let next_segment = &raw_message[..line_terminator];
            validate_header_value(next_segment)?;
            value += next_segment.trim();

            // Move to the line following the next line.
            raw_message = &raw_message[line_length..];
            consumed += line_length;
        } else {
            return Ok(Some((value, consumed)));
        }
    }
}

fn validate_header_name(text: &str) -> Result<(), Error> {
    // TODO: check if is_ascii_graphic will work here.
    if text.chars()
        .map(|c| c as u32)
        .any(|c|
            c < 33
            || c > 127
        )
    {
        Err(Error::HeaderNameContainsIllegalCharacter(text.to_string()))
    } else {
        Ok(())
    }
}

fn validate_header_value(text: &str) -> Result<(), Error> {
    // TODO: check if is_ascii_graphic will work here.
    if text.chars()
        .map(|c| c as u32)
        .all(|c|
            c == 9
            || c == 32
            || ((c > 32) && (c < 127))
        )
    {
        Ok(())
    } else {
        Err(Error::HeaderValueContainsIllegalCharacter(text.to_string()))
    }
}

#[derive(Debug, Default, Eq)]
pub struct HeaderName {
    name: String,
}

impl std::fmt::Display for HeaderName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<T> From<T> for HeaderName
    where T: AsRef<str>
{
    fn from(name: T) -> Self {
        Self{
            name: name.as_ref().to_string(),
        }
    }
}

impl PartialEq for HeaderName {
    fn eq(&self, rhs: &Self) -> bool {
        self.name.eq_ignore_ascii_case(&rhs.name)
    }
}

impl PartialEq<&str> for HeaderName {
    fn eq(&self, rhs: &&str) -> bool {
        self.name.eq_ignore_ascii_case(*rhs)
    }
}

impl PartialEq<HeaderName> for &str {
    fn eq(&self, rhs: &HeaderName) -> bool {
        self.eq_ignore_ascii_case(&rhs.name)
    }
}

impl PartialOrd for HeaderName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(
            match self.name.chars()
                .zip(other.name.chars())
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
                    self.name.len()
                        .cmp(&other.name.len())
                },
            }
        )
    }
}

#[derive(Debug, Default)]
pub struct Header {
    pub name: HeaderName,
    pub value: String,
}

#[derive(Debug, Eq, PartialEq)]
/// This enumerates the possible non-error states `MessageHeaders` can be in
/// after parsing a bit of input.
pub enum ParseStatus {
    /// The body was found at the attached byte offset in the last input
    /// string.
    Complete(usize),

    // TODO: Provide a working example below.

    /// The body was not found, but all characters up to but not including the
    /// attached byte offset were parsed.
    ///
    /// The user is expected to call `parse` again with more input, starting
    /// with the unparsed portion of the previous input string, and adding more
    /// to it.
    Incomplete(usize),
}

#[derive(Debug, Default)]
pub struct MessageHeaders {
    headers: Vec<Header>,
    line_length_limit: Option<usize>,
    valid: bool,
}

impl MessageHeaders {
    #[must_use]
    pub fn get_all(&self) -> &Vec<Header> {
        &self.headers
    }

    #[must_use]
    pub fn get_header_value<T>(&self, name: T) -> Option<String>
        where T: AsRef<str>
    {
        let name = name.as_ref();
        self.headers.iter().fold(None, |composite, header| {
            if header.name == name {
                Some(
                    if let Some(mut composite) = composite {
                        composite += ",";
                        composite += &header.value;
                        composite
                    } else {
                        header.value.clone()
                    }
                )
            } else {
                composite
            }
        })
    }

    #[must_use]
    pub fn has_header<T>(&self, name: T) -> bool
        where T: AsRef<str>
    {
        let name = name.as_ref();
        self.headers.iter().any(|header| {
            header.name == name
        })
    }

    #[must_use]
    pub fn is_valid(&self) -> bool {
        self.valid
    }

    #[must_use]
    pub fn new() -> Self {
        Self{
            headers: Vec::new(),
            line_length_limit: None,
            valid: true,
        }
    }

    pub fn parse<T>(
        &mut self,
        raw_message: T
    ) -> Result<ParseStatus, Error>
        where T: AsRef<str>
    {
        let mut offset = 0;
        let raw_message = raw_message.as_ref();
        while offset < raw_message.len() {
            // Find the end of the current line.
            //
            // TODO: Both this block and the next detect header lines that are
            // too long.  Try to rewrite this to remove the redundancy.
            let line_terminator = &raw_message[offset..].find(CRLF);
            if line_terminator.is_none() {
                if let Some(line_length_limit) = self.line_length_limit {
                    let unterminated_line_length = raw_message.len() - offset;
                    if unterminated_line_length + 2 > line_length_limit {
                        self.valid = false;
                        let end = std::cmp::min(
                            unterminated_line_length,
                            line_length_limit
                        );
                        return Err(Error::HeaderLineTooLong(
                            raw_message[offset..offset+end].to_string()
                        ));
                    }
                }
                break;
            }

            // Bail if the line is longer than the limit (if set).
            let line_terminator = line_terminator.unwrap();
            if let Some(line_length_limit) = self.line_length_limit {
                if line_terminator + CRLF.len() > line_length_limit {
                    self.valid = false;
                    return Err(Error::HeaderLineTooLong(
                        raw_message[offset..offset+line_length_limit].to_string()
                    ));
                }
            }

            // Stop if empty line is found -- this is where
            // the headers end and the body (which we don't parse,
            // but leave up to the user to handle) begins.
            if line_terminator == 0 {
                offset += CRLF.len();
                return Ok(ParseStatus::Complete(offset));
            }

            // Separate the header name from the header value.
            match separate_header_name_and_value(
                &raw_message[offset..offset+line_terminator]
            ) {
                Ok(mut header) => {
                    // Look ahead in the raw message and perform line unfolding
                    // if we see any lines that begin with whitespace.
                    offset += line_terminator + CRLF.len();
                    if let Some((value, consumed)) = unfold_header(
                        &raw_message[offset..],
                        header.value
                    )? {
                        // Remove any whitespace that might be at the beginning
                        // or end of the header value, and then store the
                        // header.
                        header.value = value.trim().to_string();
                        self.headers.push(header);
                        offset += consumed;
                    } else {
                        return Ok(ParseStatus::Incomplete(offset));
                    }
                },
                Err(error) => {
                    self.valid = false;
                    return Err(error);
                }
            }
        }
        Ok(ParseStatus::Incomplete(offset))
    }

    pub fn set_line_limit(&mut self, limit: Option<usize>) {
        self.line_length_limit = limit;
    }

}

impl std::fmt::Display for MessageHeaders {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for header in &self.headers {
            let line_buffer = format!("{}: {}", header.name, header.value);
            if let Some(line_length_limit) = self.line_length_limit {
                let mut rest = &line_buffer[..];
                while !rest.is_empty() {
                    let part = fold_header(&mut rest, line_length_limit)?;
                    write!(f, "{}{}", part, CRLF)?;
                }
            } else {
                write!(f, "{}{}", line_buffer, CRLF)?;
            }
        }
        write!(f, "{}", CRLF)
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
            let lhs = HeaderName::from(test_vector.lhs());
            let rhs = HeaderName::from(test_vector.rhs());
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
            let lhs = HeaderName::from(test_vector.lhs());
            let rhs = HeaderName::from(test_vector.rhs());
            assert_eq!(
                *test_vector.expected_result(),
                (lhs < rhs),
                "{} < {}", test_vector.lhs(), test_vector.rhs()
            );
        }
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
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        let header_collection = headers.get_all();
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
        assert_eq!(raw_message, headers.to_string());
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
            Ok(ParseStatus::Complete(raw_headers.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        let header_collection = headers.get_all();
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
        assert_eq!(raw_headers, headers.to_string());
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
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        assert_eq!(
            Some(longest_possible_poggers),
            headers.get_header_value(test_header_name)
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
            Err(Error::HeaderLineTooLong(too_long_header[0..1000].to_string())),
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
            Err(Error::HeaderLineTooLong(raw_message.clone())),
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
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert_eq!(
            Some(long_poggers),
            headers.get_header_value(test_header_name)
        );
    }

    #[test]
    fn empty_message() {
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseStatus::Incomplete(0)),
            headers.parse("")
        );
    }

}
