#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]

#[cfg(test)]
#[macro_use]
extern crate named_tuple;

mod error;

use error::Error;
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
            || c > 126
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

#[derive(Clone, Debug, Default, Eq)]
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

#[derive(Clone, Debug, Default)]
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

#[derive(Debug, Copy, Clone)]
pub enum HeaderMultiMode {
    OneLine,
    MultiLine,
}

#[derive(Clone, Debug, Default)]
pub struct MessageHeaders {
    headers: Vec<Header>,
    line_length_limit: Option<usize>,
    valid: bool,
}

impl MessageHeaders {
    pub fn add_header<N, V>(&mut self, name: N, value: V)
        where N: Into<HeaderName>, V: Into<String>
    {
        let name = name.into();
        let value = value.into();
        self.headers.push(Header{name, value});
    }

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
                self.add_header(name, values.join(","));
            },
            HeaderMultiMode::MultiLine => {
                let name = name.as_ref();
                for value in values {
                    self.add_header(name, value);
                }
            },
        }
    }

    pub fn generate(&self) -> Result<String, Error> {
        let mut raw_string = String::new();
        for header in &self.headers {
            let line_buffer = format!("{}: {}", header.name, header.value);
            if let Some(line_length_limit) = self.line_length_limit {
                let mut rest = &line_buffer[..];
                let mut skip = header.name.name.len() + 2;
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
        Ok(raw_string)
    }

    #[must_use]
    pub fn get_all(&self) -> &Vec<Header> {
        &self.headers
    }

    #[must_use]
    pub fn get_header_multi_value<T>(&self, name: T) -> Vec<String>
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

    #[must_use]
    pub fn get_header_tokens<T>(&self, name: T) -> Vec<String>
        where T: AsRef<str>
    {
        self.get_header_multi_value(name).iter()
            .flat_map(|value| {
                value.split_terminator(',')
                    .map(str::trim)
                    .map(str::to_ascii_lowercase)
            })
            .collect()
    }

    #[must_use]
    pub fn get_header_value<T>(&self, name: T) -> Option<String>
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

    pub fn has_header_token<T>(
        &self,
        name: T,
        token: T
    ) -> bool
        where T: AsRef<str>
    {
        let token = token.as_ref();
        let normalized_token = token.to_ascii_lowercase();
        self.get_header_tokens(name)
            .iter()
            .any(|token_in_header| *token_in_header == normalized_token)
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
                    let next_offset = offset + line_terminator + CRLF.len();
                    if let Some((value, consumed)) = unfold_header(
                        &raw_message[next_offset..],
                        header.value
                    )? {
                        // Remove any whitespace that might be at the beginning
                        // or end of the header value, and then store the
                        // header.
                        header.value = value.trim().to_string();
                        self.headers.push(header);
                        offset = next_offset + consumed;
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

    pub fn remove_header<N>(&mut self, name: N)
        where N: AsRef<str>
    {
        let name = name.as_ref();
        self.headers.retain(|header| {
            header.name != name
        });
    }

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
            self.add_header(name, value);
        }
    }

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
                for (i, value) in values.iter().enumerate() {
                    if i == 0 {
                        self.set_header(name, value);
                    } else {
                        self.add_header(name, value);
                    }
                }
            },
        }
    }

    pub fn set_line_limit(&mut self, limit: Option<usize>) {
        self.line_length_limit = limit;
    }

}

impl std::fmt::Display for MessageHeaders {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(raw_string) = self.generate() {
            write!(f, "{}", raw_string)
        } else {
            Ok(())
        }
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

    #[test]
    fn single_line_truncated() {
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseStatus::Incomplete(0)),
            headers.parse("User-Agent: curl")
        );
    }

    #[test]
    fn single_line_not_truncated() {
        let mut headers = MessageHeaders::new();
        let input = "User-Agent: curl\r\n\r\n";
        assert_eq!(
            Ok(ParseStatus::Complete(input.len())),
            headers.parse(input)
        );
    }

    #[test]
    fn no_headers_at_all() {
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseStatus::Complete(2)),
            headers.parse("\r\n Something Else Not Part Of The Message")
        );
        assert!(headers.is_valid());
        assert!(headers.get_all().is_empty());
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
            Ok(ParseStatus::Complete(raw_message_headers.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        assert_eq!(
            Some("www.example.com"),
            headers.get_header_value("Host").as_deref()
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
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        headers.set_header("X", "PogChamp");
        assert_eq!(
            concat!(
                "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
                "Host: www.example.com\r\n",
                "Accept-Language: en, mi\r\n",
                "X: PogChamp\r\n",
                "\r\n",
            ),
            headers.to_string()
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
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        headers.set_header("Host", "example.com");
        assert_eq!(
            concat!(
                "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n",
                "Host: example.com\r\n",
                "Accept-Language: en, mi\r\n",
                "\r\n",
            ),
            headers.to_string()
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
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        assert_eq!(
            None,
            headers.get_header_value("PePe")
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
        assert!(!headers.is_valid());
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
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        assert_eq!(
            Some("This is a test"),
            headers.get_header_value("Subject").as_deref()
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
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        assert_eq!(
            Some("This is a test"),
            headers.get_header_value("Subject").as_deref()
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
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert!(headers.is_valid());
        assert_eq!(
            Some(concat!(
                "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1",
            )),
            headers.get_header_value("Via").as_deref()
        );
        assert_eq!(
            &[
                "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
                "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
                "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1",
            ][..],
            headers.get_header_multi_value("Via")
        );
        assert_eq!(
            Some("Bob <sip:bob@biloxi.com>;tag=a6c85cf"),
            headers.get_header_value("To").as_deref()
        );
        assert_eq!(
            &[
                "Bob <sip:bob@biloxi.com>;tag=a6c85cf"
            ][..],
            headers.get_header_multi_value("To")
        );
        assert!(
            headers.get_header_multi_value("PogChamp").is_empty()
        );
    }

    #[test]
    fn set_header_multi_valueple_values() {
        let via: Vec<String> = [
            "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
        ].iter().map(|s| String::from(*s)).collect();
        let to : Vec<String>= [
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
            )),
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
            )),
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
        headers.add_header("Via", "Trickster");
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                    "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                    "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "Via: Trickster\r\n",
                "\r\n",
            )),
            headers.generate().as_deref()
        );
        headers.set_header("Via", "Kappa");
        assert_eq!(
            Ok(concat!(
                "Via: Kappa\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "\r\n",
            )),
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
            )),
            headers.generate().as_deref()
        );
        headers.add_header("From", "Alice <sip:alice@atlanta.com>;tag=1928301774");
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,",
                    "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,",
                    "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "\r\n",
            )),
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
            )),
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
            )),
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
        headers.add_header("From", "Alice <sip:alice@atlanta.com>;tag=1928301774");
        assert_eq!(
            Ok(concat!(
                "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n",
                "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n",
                "Via: SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n",
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n",
                "\r\n",
            )),
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
            )),
            headers.generate().as_deref()
        );
        headers.remove_header("Via");
        assert_eq!(
            Ok(concat!(
                "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n",
                "\r\n",
            )),
            headers.generate().as_deref()
        );
    }

    #[test]
    fn get_header_tokens() {
        let raw_message = concat!(
            "Foo: bar, Spam,  heLLo\r\n",
            "Bar: Foo \r\n",
            "Spam:   \t  \r\n",
            "\r\n",
        );
        let mut headers = MessageHeaders::new();
        assert_eq!(
            Ok(ParseStatus::Complete(raw_message.len())),
            headers.parse(raw_message)
        );
        assert_eq!(
            &[
                "bar",
                "spam",
                "hello",
            ][..],
            headers.get_header_tokens("Foo")
        );
        assert_eq!(
            &[
                "foo",
            ][..],
            headers.get_header_tokens("Bar")
        );
        assert_eq!(
            &[] as &[String],
            headers.get_header_tokens("Spam")
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
            Ok(ParseStatus::Complete(raw_message.len())),
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
        assert_eq!(Some("Bar"), original_headers.get_header_value("Foo").as_deref());
        assert_eq!(Some("World"), original_headers.get_header_value("Hello").as_deref());
        assert_eq!(Some("Bar"), headers_copy.get_header_value("Foo").as_deref());
        assert_eq!(Some("PePe"), headers_copy.get_header_value("Hello").as_deref());
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
            Ok(ParseStatus::Incomplete(raw_message_pieces[0].len())),
            headers.parse(
                String::from(raw_message_pieces[0])
                + raw_message_pieces[1]
            )
        );
        assert_eq!(
            Ok(ParseStatus::Complete(
                raw_message_pieces[1].len()
                + raw_message_pieces[2].len()
                + raw_message_pieces[3].len()
            )),
            headers.parse(
                String::from(raw_message_pieces[1])
                + raw_message_pieces[2]
                + raw_message_pieces[3]
            )
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
            .for_each(|(expected_header, actual_header)| {
                assert_eq!(*expected_header.name(), actual_header.name);
                assert_eq!(*expected_header.value(), actual_header.value);
            });
        assert!(headers.has_header("Host"));
        assert!(!headers.has_header("Foobar"));
        assert_eq!(
            Ok(raw_message_pieces.iter().copied().collect::<String>()),
            headers.generate()
        );
    }

}
