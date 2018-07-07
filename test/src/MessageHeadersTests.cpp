/**
 * @file MessageHeadersTests.cpp
 *
 * This module contains the unit tests of the
 * MessageHeaders::MessageHeaders class.
 *
 * © 2018 by Richard Walters
 */

#include <gtest/gtest.h>
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
    ASSERT_TRUE(headers.ParseRawMessage(rawMessage));
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
    ASSERT_TRUE(headers.ParseRawMessage(rawMessage, bodyOffset));
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
    ASSERT_TRUE(headers.ParseRawMessage(rawMessage));
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
    ASSERT_FALSE(headers.ParseRawMessage(rawMessage));
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
    ASSERT_TRUE(headers.ParseRawMessage(rawMessage));
    ASSERT_EQ(valueForHeaderLineLongerThan1000Characters, headers.GetHeaderValue(testHeaderName));
}

TEST(MessageHeadersTests, GetValueOfPresentHeader) {
    MessageHeaders::MessageHeaders msg;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_TRUE(msg.ParseRawMessage(rawMessage));
    ASSERT_EQ("www.example.com", msg.GetHeaderValue("Host"));
}

TEST(MessageHeadersTests, SetHeaderAdd) {
    MessageHeaders::MessageHeaders msg;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_TRUE(msg.ParseRawMessage(rawMessage));
    msg.SetHeader("X", "PogChamp");
    ASSERT_EQ(
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "X: PogChamp\r\n"
        "\r\n",
        msg.GenerateRawHeaders()
    );
}

TEST(MessageHeadersTests, SetHeaderReplace) {
    MessageHeaders::MessageHeaders msg;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_TRUE(msg.ParseRawMessage(rawMessage));
    msg.SetHeader("Host", "example.com");
    ASSERT_EQ(
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n",
        msg.GenerateRawHeaders()
    );
}

TEST(MessageHeadersTests, GetValueOfMissingHeader) {
    MessageHeaders::MessageHeaders msg;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_TRUE(msg.ParseRawMessage(rawMessage));
    ASSERT_EQ("FeelsBadMan", msg.GetHeaderValue("PePe"));
}

TEST(MessageHeadersTests, HeaderWithNonAsciiCharacterInName) {
    MessageHeaders::MessageHeaders msg;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Feels Bad Man: LUL\r\n"
        "Accept-Language: en, mi\r\n"
        "\r\n"
    );
    ASSERT_FALSE(msg.ParseRawMessage(rawMessage));
}

TEST(MessageHeadersTests, HeaderValueUnfolding) {
    MessageHeaders::MessageHeaders msg;
    const std::string rawMessage = (
        "User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n"
        "Host: www.example.com\r\n"
        "Accept-Language: en, mi\r\n"
        "Subject: This\r\n"
        " is a test\r\n"
        "\r\n"
    );
    ASSERT_TRUE(msg.ParseRawMessage(rawMessage));
    ASSERT_EQ("This is a test", msg.GetHeaderValue("Subject"));
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
        MessageHeaders::MessageHeaders msg;
        msg.SetLineLimit(12);
        msg.SetHeader(headerName, testVector.headerValue);
        const auto rawHeaders = msg.GenerateRawHeaders();
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
        MessageHeaders::MessageHeaders msg;
        msg.SetHeader(testVector.headerName, "HeyGuys");
        for (const auto& alternative: testVector.shouldAlsoMatch) {
            ASSERT_TRUE(msg.HasHeader(alternative));
        }
    }
}
