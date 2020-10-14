#![warn(clippy::pedantic)]

#[cfg(test)]
#[macro_use]
extern crate named_tuple;

#[derive(Debug, Eq, Ord)]
pub struct HeaderName {
    name: String,
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

pub struct Header {
    pub name: HeaderName,
    pub value: String,
}

#[derive(PartialEq, Eq, Debug)]
pub enum MessageHeadersState {
    Complete,
}

#[derive(Debug, Default)]
pub struct MessageHeaders {
}

impl MessageHeaders {
    #[must_use]
    pub fn generate_raw_headers(&self) -> String {
        todo!()
    }

    #[must_use]
    pub fn get_all(&self) -> Vec<Header> {
        todo!()
    }

    #[must_use]
    pub fn has_header<T>(&self, name: T) -> bool
        where T: AsRef<str>
    {
        todo!()
    }

    #[must_use]
    pub fn is_valid(&self) -> bool {
        todo!()
    }

    #[must_use]
    pub fn new() -> Self {
        Self{
        }
    }

    pub fn parse_raw_message<T>(
        &mut self,
        _raw_message: T
    ) -> MessageHeadersState
        where T: AsRef<str>
    {
        todo!()
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
            MessageHeadersState::Complete,
            headers.parse_raw_message(raw_message)
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
        assert_eq!(raw_message, headers.generate_raw_headers());
    }

}
