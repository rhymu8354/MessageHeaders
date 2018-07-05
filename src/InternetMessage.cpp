/**
 * @file InternetMessage.cpp
 *
 * This module contains the implementation of the InternetMessage::InternetMessage class.
 *
 * © 2018 by Richard Walters
 */

#include <InternetMessage/InternetMessage.hpp>
#include <sstream>
#include <string>

namespace {

    /**
     * These are the characters that are considered whitespace and
     * should be stripped off by the Strip() function.
     */
    constexpr const char* WHITESPACE = " \r\n\t";

    /**
     * This function returns a copy of the given string, with any whitespace
     * at the beginning and end stripped off.
     *
     * @param[in] s
     *     This is the string to strip.
     *
     * @return
     *     The stripped string is returned.
     */
    std::string StripMarginWhitespace(const std::string& s) {
        const auto marginLeft = s.find_first_not_of(WHITESPACE);
        const auto marginRight = s.find_last_not_of(WHITESPACE);
        if (marginLeft == std::string::npos) {
            return "";
        } else {
            return s.substr(marginLeft, marginRight - marginLeft + 1);
        }
    }

}

namespace InternetMessage {

    InternetMessage::Header::Header(
        const HeaderName& newName,
        const HeaderValue& newValue
    )
        : name(newName)
        , value(newValue)
    {
    }

    /**
     * This contains the private properties of a InternetMessage instance.
     */
    struct InternetMessage::Impl {
        /**
         * These are the headers of the message.
         */
        Headers headers;

        /**
         * This is the body of the message.
         */
        std::string body;
    };

    InternetMessage::~InternetMessage() = default;

    InternetMessage::InternetMessage()
        : impl_(new Impl)
    {
    }

    bool InternetMessage::ParseRawMessage(const std::string& rawMessage) {
        size_t offset = 0;
        while(offset < rawMessage.length()) {
            auto lineTerminator = rawMessage.find("\r\n", offset);
            if (lineTerminator == std::string::npos) {
                break;
            }
            if (lineTerminator - offset > 998) {
                return false;
            }
            if (lineTerminator == offset) {
                offset += 2;
                break;
            }
            auto nameValueDelimiter = rawMessage.find(':', offset);
            if (nameValueDelimiter == std::string::npos) {
                return false;
            }
            HeaderName name;
            HeaderValue value;
            name = rawMessage.substr(offset, nameValueDelimiter - offset);
            for (auto c: name) {
                if (
                    (c < 33)
                    || (c > 126)
                ) {
                    return false;
                }
            }
            value = StripMarginWhitespace(
                rawMessage.substr(
                    nameValueDelimiter + 1,
                    lineTerminator - nameValueDelimiter - 1
                )
            );
            impl_->headers.emplace_back(name, value);
            offset = lineTerminator + 2;
        }
        impl_->body = rawMessage.substr(offset);
        bool lastCR = false;
        for (auto c: impl_->body) {
            if (c == '\r') {
                lastCR = true;
            } else if (
                (c == '\n') == !lastCR
            ) {
                return false;
            } else {
                lastCR = false;
            }
        }
        if (lastCR) {
            return false;
        }
        return true;
    }

    auto InternetMessage::GetHeaders() const -> Headers {
        return impl_->headers;
    }

    bool InternetMessage::HasHeader(const HeaderName& name) const {
        for (const auto& header: impl_->headers) {
            if (header.name == name) {
                return true;
            }
        }
        return false;
    }

    auto InternetMessage::GetHeaderValue(const HeaderName& name) const -> HeaderValue {
        for (const auto& header: impl_->headers) {
            if (header.name == name) {
                return header.value;
            }
        }
        return "FeelsBadMan";
    }

    std::string InternetMessage::GetBody() const {
        return impl_->body;
    }

    std::string InternetMessage::GenerateRawMessage() const {
        std::ostringstream rawMessage;
        for (const auto& header: impl_->headers) {
            rawMessage << header.name << ": " << header.value << "\r\n";
        }
        rawMessage << "\r\n";
        rawMessage << impl_->body;
        return rawMessage.str();
    }

}
