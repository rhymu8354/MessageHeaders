/**
 * @file MessageHeadersTests.cpp
 *
 * This module contains the unit tests of the
 * MessageHeaders::MessageHeaders class.
 *
 * © 2018 by Richard Walters
 */

#include <gtest/gtest.h>
#include <limits>
#include <MessageHeaders/MessageHeaders.hpp>
#include <string>
#include <vector>

TEST(MessageHeadersTests, HttpClientRequestMessage) {
    MessageHeaders::MessageHeaders headers;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_TRUE(headers.IsValid());
    const auto headerCollection = headers.GetAll();
    struct ExpectedHeader {
        std::string name;
        std::string value;
    };
    const std::vector< ExpectedHeader > expectedHeaders{
        {"User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3"},
        {"Host", "www.example.com"},
        {"Accept-Language", "en, mi"},
    };
    ASSERT_EQ(expectedHeaders.size(), headerCollection.size());
    for (size_t i = 0; i < expectedHeaders.size(); ++i) {
        ASSERT_EQ(expectedHeaders[i].name, headerCollection[i].name);
        ASSERT_EQ(expectedHeaders[i].value, headerCollection[i].value);
    }
    ASSERT_TRUE(headers.HasHeader("Host"));
    ASSERT_FALSE(headers.HasHeader("Foobar"));
    ASSERT_EQ(rawMessage, headers.GenerateRawHeaders());
}

TEST(MessageHeadersTests, HttpServerResponseMessage) {
    MessageHeaders::MessageHeaders headers;
    const std::string rawHeaders = (
        "Date: Mon, 27 Jul 2009 12:28:53 GMT\r\n"
        "Server: Apache\r\n"
        "Last-Modified: Wed, 22 Jul 2009 19:15:56 GMT\r\n"
        "ETag: \"34aa387-d-1568eb00\"\r\n"
        "Accept-Ranges: bytes\r\n"
        "Content-Length: 51\r\n"
        "Vary: Accept-Encoding\r\n"
        "Content-Type: text/plain\r\n"
        "\r\n"
    );
    const std::string rawMessage = (
        rawHeaders
        + "Hello World! My payload includes a trailing CRLF.\r\n"
    );
    size_t bodyOffset;
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage, bodyOffset)
    );
    ASSERT_TRUE(headers.IsValid());
    ASSERT_EQ(rawHeaders.length(), bodyOffset);
    const auto headerCollection = headers.GetAll();
    struct ExpectedHeader {
        std::string name;
        std::string value;
    };
    const std::vector< ExpectedHeader > expectedHeaders{
        {"Date", "Mon, 27 Jul 2009 12:28:53 GMT"},
        {"Server", "Apache"},
        {"Last-Modified", "Wed, 22 Jul 2009 19:15:56 GMT"},
        {"ETag", "\"34aa387-d-1568eb00\""},
        {"Accept-Ranges", "bytes"},
        {"Content-Length", "51"},
        {"Vary", "Accept-Encoding"},
        {"Content-Type", "text/plain"},
    };
    ASSERT_EQ(expectedHeaders.size(), headerCollection.size());
    for (size_t i = 0; i < expectedHeaders.size(); ++i) {
        ASSERT_EQ(expectedHeaders[i].name, headerCollection[i].name);
        ASSERT_EQ(expectedHeaders[i].value, headerCollection[i].value);
    }
    ASSERT_TRUE(headers.HasHeader("Last-Modified"));
    ASSERT_FALSE(headers.HasHeader("Foobar"));
    ASSERT_EQ(rawHeaders, headers.GenerateRawHeaders());
}

TEST(MessageHeadersTests, HeaderLineAlmostTooLong) {
    MessageHeaders::MessageHeaders headers;
    headers.SetLineLimit(1000);
    const std::string testHeaderName("X-Poggers");
    const std::string testHeaderNameWithDelimiters = testHeaderName + ": ";
    const std::string longestPossiblePoggers(998 - testHeaderNameWithDelimiters.length(), 'X');
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        + testHeaderNameWithDelimiters + longestPossiblePoggers + "\r\n"
        + "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_TRUE(headers.IsValid());
    ASSERT_EQ(longestPossiblePoggers, headers.GetHeaderValue(testHeaderName));
}

TEST(MessageHeadersTests, HeaderLineTooLong) {
    MessageHeaders::MessageHeaders headers;
    headers.SetLineLimit(1000);
    const std::string testHeaderName("X-Poggers");
    const std::string testHeaderNameWithDelimiters = testHeaderName + ": ";
    const std::string valueIsTooLong(999 - testHeaderNameWithDelimiters.length(), 'X');
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        + testHeaderNameWithDelimiters + valueIsTooLong + "\r\n"
        + "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Error,
        headers.ParseRawMessage(rawMessage)
    );
}

TEST(MessageHeadersTests, HeaderLineTooLongAndNotTerminated) {
    MessageHeaders::MessageHeaders headers;
    headers.SetLineLimit(1000);
    const std::string testHeaderName("X-Poggers");
    const std::string testHeaderNameWithDelimiters = testHeaderName + ": ";
    const std::string valueIsTooLong(999 - testHeaderNameWithDelimiters.length(), 'X');
    const std::string rawMessage = (
        testHeaderNameWithDelimiters + valueIsTooLong
    );
    size_t messageEnd = std::numeric_limits< size_t >::max();
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Error,
        headers.ParseRawMessage(rawMessage, messageEnd)
    );
}

TEST(MessageHeadersTests, HeaderLineOver1000CharactersAllowedByDefault) {
    MessageHeaders::MessageHeaders headers;
    const std::string testHeaderName("X-Poggers");
    const std::string testHeaderNameWithDelimiters = testHeaderName + ": ";
    const std::string valueForHeaderLineLongerThan1000Characters(999 - testHeaderNameWithDelimiters.length(), 'X');
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        + testHeaderNameWithDelimiters + valueForHeaderLineLongerThan1000Characters + "\r\n"
        + "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_TRUE(headers.IsValid());
    ASSERT_EQ(valueForHeaderLineLongerThan1000Characters, headers.GetHeaderValue(testHeaderName));
}

TEST(MessageHeadersTests, EmptyMessage) {
    MessageHeaders::MessageHeaders headers;
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Incomplete,
        headers.ParseRawMessage("")
    );
}

TEST(MessageHeadersTests, SingleLineTruncated) {
    MessageHeaders::MessageHeaders headers;
    size_t messageEnd;
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Incomplete,
        headers.ParseRawMessage("User-Agent: curl", messageEnd)
    );
    ASSERT_EQ(0, messageEnd);
}

TEST(MessageHeadersTests, SingleLineNotTruncated) {
    MessageHeaders::MessageHeaders headers;
    size_t messageEnd;
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Incomplete,
        headers.ParseRawMessage("User-Agent: curl\r\n", messageEnd)
    );
    ASSERT_EQ(18, messageEnd);
}

TEST(MessageHeadersTests, NoHeadersAtAll) {
    MessageHeaders::MessageHeaders headers;
    size_t messageEnd;
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage("\r\n Something Else Not Part Of The Message", messageEnd)
    );
    ASSERT_EQ(2, messageEnd);
    ASSERT_TRUE(headers.IsValid());
    ASSERT_TRUE(headers.GetAll().empty());
}

TEST(MessageHeadersTests, GetValueOfPresentHeader) {
    MessageHeaders::MessageHeaders headers;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    size_t messageEnd;
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage + " Something Else Not Part Of The Message", messageEnd)
    );
    ASSERT_EQ(rawMessage.length(), messageEnd);
    ASSERT_TRUE(headers.IsValid());
    ASSERT_EQ("www.example.com", headers.GetHeaderValue("Host"));
}

TEST(MessageHeadersTests, SetHeaderAdd) {
    MessageHeaders::MessageHeaders headers;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_TRUE(headers.IsValid());
    headers.SetHeader("X", "PogChamp");
    ASSERT_EQ(
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "X: PogChamp\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
}

TEST(MessageHeadersTests, SetHeaderReplace) {
    MessageHeaders::MessageHeaders headers;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_TRUE(headers.IsValid());
    headers.SetHeader("Host", "example.com");
    ASSERT_EQ(
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
}

TEST(MessageHeadersTests, GetValueOfMissingHeader) {
    MessageHeaders::MessageHeaders headers;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_TRUE(headers.IsValid());
    ASSERT_EQ("", headers.GetHeaderValue("PePe"));
}

TEST(MessageHeadersTests, HeaderWithNotPermittedCharacterInName) {
    MessageHeaders::MessageHeaders headers;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Feels Bad Man: LUL\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    size_t messageEnd = std::numeric_limits< size_t >::max();
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage, messageEnd)
    );
    ASSERT_FALSE(headers.IsValid());
    ASSERT_EQ(rawMessage.length(), messageEnd);
}

TEST(MessageHeadersTests, HeaderValueUnfolding) {
    MessageHeaders::MessageHeaders headers;
    std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "Subject: This\r\n"
        " is a test\r\n"
        "\r\n"
    );
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_TRUE(headers.IsValid());
    ASSERT_EQ("This is a test", headers.GetHeaderValue("Subject"));
    headers = MessageHeaders::MessageHeaders();
    rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "Subject: This\r\n"
        "    is a test\r\n"
        "\r\n"
    );
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_TRUE(headers.IsValid());
    ASSERT_EQ("This is a test", headers.GetHeaderValue("Subject"));
}

TEST(MessageHeadersTests, FoldLineThatWouldExceedLimit) {
    const std::string headerName = "X";
    struct TestVector {
        std::string headerValue;
        std::vector< std::string > expectedLines;
    };
    std::vector< TestVector > testVectors{
        // ...................... {"..........", "..........", "..........", ""}}
        {"Hello!",                {"X: Hello!" ,                             ""}},
        {"Hello!!",               {"X: Hello!!",                             ""}},
        {"Hello!!!",              {                                          ""}},
        {"Hello, World!",         {"X: Hello," , " World!"   ,               ""}},
        {"This is even longer!",  {"X: This is", " even"     , " longer!"  , ""}},
        {"This is even long er!", {"X: This is", " even long", " er!"      , ""}},
        {"This is evenlonger!",   {                                          ""}},
        {"sadfjkasdfjlkasdfjla",  {                                          ""}},
    };
    size_t index = 0;
    for (const auto& testVector: testVectors) {
        MessageHeaders::MessageHeaders headers;
        headers.SetLineLimit(12);
        headers.SetHeader(headerName, testVector.headerValue);
        const auto rawHeaders = headers.GenerateRawHeaders();
        std::vector< std::string > actualLines;
        size_t offset = 0;
        while(offset < rawHeaders.length()) {
            auto lineTerminator = rawHeaders.find("\r\n", offset);
            if (lineTerminator == std::string::npos) {
                break;
            }
            const auto line = rawHeaders.substr(
                offset,
                lineTerminator - offset
            );
            actualLines.push_back(line);
            offset = lineTerminator + 2;
        }
        ASSERT_EQ(testVector.expectedLines, actualLines) << index;
        ++index;
    }
}

TEST(MessageHeadersTests, HeaderNamesShouldBeCaseInsensive) {
    struct TestVector {
        std::string headerName;
        std::vector< std::string > shouldAlsoMatch;
    };
    std::vector< TestVector > testVectors{
        {"Content-Type", {"content-type", "CONTENT-TYPE", "Content-type", "CoNtENt-TYpe"}},
        {"ETag", {"etag", "ETAG", "Etag", "eTag", "etaG"}},
    };
    size_t index = 0;
    for (const auto& testVector: testVectors) {
        MessageHeaders::MessageHeaders headers;
        headers.SetHeader(testVector.headerName, "HeyGuys");
        for (const auto& alternative: testVector.shouldAlsoMatch) {
            ASSERT_TRUE(headers.HasHeader(alternative));
        }
    }
}

TEST(MessageHeadersTests, GetHeaderMultipleValues) {
    const std::string rawMessage = (
        "Via: SIP/2.0/UDP server10.biloxi.com\r\n"
        "    ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n"
        "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com\r\n"
        "    ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n"
        "Via: SIP/2.0/UDP pc33.atlanta.com\r\n"
        "    ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "\r\n"
    );
    MessageHeaders::MessageHeaders headers;
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_TRUE(headers.IsValid());
    ASSERT_EQ(
        "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,"
        "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,"
        "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1",
        headers.GetHeaderValue("Via")
    );
    ASSERT_EQ(
        (std::vector< MessageHeaders::MessageHeaders::HeaderValue >{
            "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
        }),
        headers.GetHeaderMultiValue("Via")
    );
    ASSERT_EQ(
        "Bob <sip:bob@biloxi.com>;tag=a6c85cf",
        headers.GetHeaderValue("To")
    );
    ASSERT_EQ(
        (std::vector< MessageHeaders::MessageHeaders::HeaderValue >{
            "Bob <sip:bob@biloxi.com>;tag=a6c85cf"
        }),
        headers.GetHeaderMultiValue("To")
    );
    ASSERT_EQ(
        (std::vector< MessageHeaders::MessageHeaders::HeaderValue >{}),
        headers.GetHeaderMultiValue("PogChamp")
    );
}

TEST(MessageHeadersTests, SetHeaderMultipleValues) {
    std::vector< MessageHeaders::MessageHeaders::HeaderValue > via{
        "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
        "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
        "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
    };
    std::vector< MessageHeaders::MessageHeaders::HeaderValue > to{
        "Bob <sip:bob@biloxi.com>;tag=a6c85cf"
    };
    MessageHeaders::MessageHeaders headers;
    headers.SetHeader("Via", via, true);
    headers.SetHeader("To", to, true);
    headers.SetHeader("FeelsBadMan", {}, true);
    ASSERT_EQ(
        "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,"
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,"
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
    headers = MessageHeaders::MessageHeaders();
    headers.SetHeader("Via", via, false);
    headers.SetHeader("To", to, false);
    headers.SetHeader("FeelsBadMan", {}, false);
    ASSERT_EQ(
        "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n"
        "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n"
        "Via: SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
}

TEST(MessageHeadersTests, SetHeaderShouldReplaceAllPreviousValues) {
    std::vector< MessageHeaders::MessageHeaders::HeaderValue > via{
        "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
        "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
        "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
    };
    std::vector< MessageHeaders::MessageHeaders::HeaderValue > to{
        "Bob <sip:bob@biloxi.com>;tag=a6c85cf"
    };
    MessageHeaders::MessageHeaders headers;
    headers.SetHeader("Via", via, true);
    headers.SetHeader("To", "Bob <sip:bob@biloxi.com>;tag=a6c85cf");
    headers.SetHeader("From", "Alice <sip:alice@atlanta.com>;tag=1928301774");
    headers.AddHeader("Via", "Trickster");
    ASSERT_EQ(
        "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,"
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,"
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n"
        "Via: Trickster\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
    headers.SetHeader("Via", "Kappa");
    ASSERT_EQ(
        "Via: Kappa\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
}

TEST(MessageHeadersTests, AddHeader) {
    std::vector< MessageHeaders::MessageHeaders::HeaderValue > via{
        "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
        "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
        "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
    };
    MessageHeaders::MessageHeaders headers;
    headers.SetHeader("Via", via, true);
    headers.SetHeader("To", "Bob <sip:bob@biloxi.com>;tag=a6c85cf");
    ASSERT_EQ(
        "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,"
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,"
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
    headers.AddHeader("From", "Alice <sip:alice@atlanta.com>;tag=1928301774");
    ASSERT_EQ(
        "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,"
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,"
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
    std::vector< MessageHeaders::MessageHeaders::HeaderValue > x_pepe{
        "<3",
        "SeemsGood",
    };
    headers.AddHeader("X-PePe", x_pepe, true);
    ASSERT_EQ(
        "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,"
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,"
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n"
        "X-PePe: <3,SeemsGood\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
    headers.AddHeader("To", {"Carol"}, true);
    ASSERT_EQ(
        "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3,"
            "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2,"
            "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n"
        "X-PePe: <3,SeemsGood\r\n"
        "To: Carol\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
}

TEST(MessageHeadersTests, RemoveHeader) {
    std::vector< MessageHeaders::MessageHeaders::HeaderValue > via{
        "SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3",
        "SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2",
        "SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
    };
    MessageHeaders::MessageHeaders headers;
    headers.SetHeader("Via", via, false);
    headers.SetHeader("To", "Bob <sip:bob@biloxi.com>;tag=a6c85cf");
    headers.AddHeader("From", "Alice <sip:alice@atlanta.com>;tag=1928301774");
    ASSERT_EQ(
        "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n"
        "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n"
        "Via: SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "From: Alice <sip:alice@atlanta.com>;tag=1928301774\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
    headers.RemoveHeader("From");
    ASSERT_EQ(
        "Via: SIP/2.0/UDP server10.biloxi.com ;branch=z9hG4bKnashds8;received=192.0.2.3\r\n"
        "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2\r\n"
        "Via: SIP/2.0/UDP pc33.atlanta.com ;branch=z9hG4bK776asdhds ;received=192.0.2.1\r\n"
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
    headers.RemoveHeader("Via");
    ASSERT_EQ(
        "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf\r\n"
        "\r\n",
        headers.GenerateRawHeaders()
    );
}

TEST(MessageHeadersTests, GetHeaderTokens) {
    const std::string rawMessage = (
        "Foo: bar, Spam,  heLLo\r\n"
        "Bar: Foo \r\n"
        "Spam:   \t  \r\n"
        "\r\n"
    );
    MessageHeaders::MessageHeaders headers;
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    ASSERT_EQ(
        (std::vector< std::string >{
            "bar",
            "spam",
            "hello",
        }),
        headers.GetHeaderTokens("Foo")
    );
    ASSERT_EQ(
        (std::vector< std::string >{
            "foo",
        }),
        headers.GetHeaderTokens("Bar")
    );
    ASSERT_EQ(
        (std::vector< std::string >{
        }),
        headers.GetHeaderTokens("Spam")
    );
}

TEST(MessageHeadersTests, HasHeaderToken) {
    const std::string rawMessage = (
        "Foo: bar, Spam,  heLLo\r\n"
        "Bar: Foo \r\n"
        "Spam:   \t  \r\n"
        "\r\n"
    );
    MessageHeaders::MessageHeaders headers;
    ASSERT_EQ(
        MessageHeaders::MessageHeaders::State::Complete,
        headers.ParseRawMessage(rawMessage)
    );
    EXPECT_TRUE(headers.HasHeaderToken("Foo", "bar"));
    EXPECT_TRUE(headers.HasHeaderToken("Foo", "Bar"));
    EXPECT_TRUE(headers.HasHeaderToken("Foo", "spam"));
    EXPECT_TRUE(headers.HasHeaderToken("Foo", "hello"));
    EXPECT_FALSE(headers.HasHeaderToken("Foo", "xyz"));
    EXPECT_FALSE(headers.HasHeaderToken("Foo", "secret_to_the_universe"));
    EXPECT_TRUE(headers.HasHeaderToken("Bar", "foo"));
    EXPECT_FALSE(headers.HasHeaderToken("Bar", "spam"));
    EXPECT_FALSE(headers.HasHeaderToken("Spam", "foo"));
    EXPECT_FALSE(headers.HasHeaderToken("Spam", "spam"));
}

TEST(MessageHeadersTests, CopyHeadersInConstructor) {
    MessageHeaders::MessageHeaders originalHeaders;
    originalHeaders.SetHeader("Foo", "Bar");
    originalHeaders.SetHeader("Hello", "World");
    MessageHeaders::MessageHeaders headersCopy(originalHeaders);
    headersCopy.SetHeader("Hello", "PePe");
    EXPECT_EQ("Bar", originalHeaders.GetHeaderValue("Foo"));
    EXPECT_EQ("World", originalHeaders.GetHeaderValue("Hello"));
    EXPECT_EQ("Bar", headersCopy.GetHeaderValue("Foo"));
    EXPECT_EQ("PePe", headersCopy.GetHeaderValue("Hello"));
}

TEST(MessageHeadersTests, CopyHeadersInAssignment) {
    MessageHeaders::MessageHeaders originalHeaders;
    originalHeaders.SetHeader("Foo", "Bar");
    originalHeaders.SetHeader("Hello", "World");
    MessageHeaders::MessageHeaders headersCopy;
    headersCopy.SetHeader("Foo", "XXX");
    headersCopy = originalHeaders;
    headersCopy.SetHeader("Hello", "PePe");
    EXPECT_EQ("Bar", originalHeaders.GetHeaderValue("Foo"));
    EXPECT_EQ("World", originalHeaders.GetHeaderValue("Hello"));
    EXPECT_EQ("Bar", headersCopy.GetHeaderValue("Foo"));
    EXPECT_EQ("PePe", headersCopy.GetHeaderValue("Hello"));
}
