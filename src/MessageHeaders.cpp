/**
 * @file MessageHeaders.cpp
 *
 * This module contains the implementation of the MessageHeaders::MessageHeaders class.
 *
 * © 2018 by Richard Walters
 */

#include <ctype.h>
#include <functional>
#include <MessageHeaders/MessageHeaders.hpp>
#include <sstream>
#include <string>

namespace {

    /**
     * These are the characters that are considered whitespace and
     * should be stripped off by the Strip() function.
     *
     * Name of "WSP" chosen to match the symbol name from
     * RFC 5322 (https://tools.ietf.org/html/rfc5322) which refers
     * to this specific character set.
     */
    const std::string WSP = " \t";

    /**
     * This is the required line terminator for internet message header lines.
     */
    const std::string CRLF = "\r\n";

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
        const auto marginLeft = s.find_first_not_of(WSP);
        const auto marginRight = s.find_last_not_of(WSP);
        if (marginLeft == std::string::npos) {
            return "";
        } else {
            return s.substr(marginLeft, marginRight - marginLeft + 1);
        }
    }

    /**
     * This function takes a string and swaps all upper-case characters
     * with their lower-case equivalents, returning the result.
     *
     * @param[in] inString
     *     This is the string to be normalized.
     *
     * @return
     *     The normalized string is returned.  All upper-case characters
     *     are replaced with their lower-case equivalents.
     */
    std::string NormalizeCaseInsensitiveString(const std::string& inString) {
        std::string outString;
        for (char c: inString) {
            outString.push_back(tolower(c));
        }
        return outString;
    }

    /**
     * This function separates the tokens in a string.
     *
     * @param[in] s
     *     This is the string to split into tokens.
     *
     * @param[in,out] tokens
     *     This is where to put the tokens split from the string.
     *     The new tokens are pushed onto the end.
     *
     * @return
     *     The list of tokens split from the string is returned.
     */
    void SplitTokens(
        const std::string& s,
        std::vector< std::string >& tokens
    ) {
        auto remainder = StripMarginWhitespace(s);
        while (!remainder.empty()) {
            auto delimiter = remainder.find_first_of(',');
            if (delimiter == std::string::npos) {
                tokens.push_back(NormalizeCaseInsensitiveString(remainder));
                remainder.clear();
            } else {
                tokens.push_back(StripMarginWhitespace(remainder.substr(0, delimiter)));
                remainder = NormalizeCaseInsensitiveString(
                    StripMarginWhitespace(remainder.substr(delimiter + 1))
                );
            }
        }
    }

    /**
     * This function determines whether or not one string ends with another.
     *
     * @param[in] s
     *     This is the string to check and see if it ends with
     *     the other string.
     *
     * @param[in] e
     *     This is the string to look for on the end of the first string.
     *
     * @return
     *     An indication of whether or not one string ends with another
     *     is returned.
     */
    bool EndsWith(
        const std::string& s,
        const std::string& e
    ) {
        return (
            (s.length() < e.length())
            || (s.substr(s.length() - e.length()) != e)
        );
    }

    /**
     * This function returns a single string which contains
     * all the given strings, with a copy of the given delimiter
     * placed between each original string.
     *
     * @param[in] strings
     *     These are the strings to combine.
     *
     * @param[in] delimiter
     *     This is the string to place between each original string.
     *
     * @return
     *     The combined string is returned.
     */
    std::string CombineStrings(
        const std::vector< std::string >& strings,
        const std::string& delimiter
    ) {
        bool isFirstValue = true;
        std::string composite;
        for (const auto& s: strings) {
            if (isFirstValue) {
                isFirstValue = false;
            } else {
                composite += delimiter;
            }
            composite += s;
        }
        return composite;
    }

    /**
     * This function determines whether or not the given character
     * is an invisible ASCII character (e.g. space or control character).
     *
     * @param[in] c
     *     This is the character to test.
     *
     * @return
     *     An indication of whether or not the given character is an
     *     invisible ASCII character (e.g. space or control character)
     *     is returned.
     */
    bool IsInvisibleAscii(char c) {
        return (
            (c < 33)
            || (c > 126)
        );
    }

    /**
     * This is the type of function that is used as the strategy to
     * determine where to break a long string into two smaller strings.
     *
     * @param[in] s
     *     This is the string which we are considering breaking.
     *
     * @param[in] startOffset
     *     This is the offset into the given string of the beginning of
     *     the partthat is under consideration for breaking.
     *
     * @param[out] breakOffset
     *     When the string is broken into two parts, this is the offset
     *     in the original string marking the end of the first part.
     *
     * @param[out] nextOffset
     *     When the string is broken into two parts, this is the offset
     *     in the original string marking the beginning of the second part.
     *
     * @return
     *     An indication of whether or not the string can be
     *     broken is returned.
     */
    typedef std::function<
        bool(
            const std::string& s,
            size_t startOffset,
            size_t& breakOffset,
            size_t& nextOffset
        )
    > StringBreakingStrategy;

    /**
     * This method breaks up the given input line into multiple output lines,
     * as needed, to ensure that no output line is longer than the given
     * line limit, including line terminators.
     *
     * @param[in] input
     *     This is the line to fold if necessary.
     *
     * @param[in] terminator
     *     This is the character sequence that separates lines.
     *
     * @param[in] continuator
     *     This is the string to preprend to each output
     *     line after the first.
     *
     * @param[in] inputSplittingStrategy
     *     This is the function to call that will determine
     *     where (if anywhere) to break up the input line.
     *
     * @return
     *     The output lines are returned.
     *
     * @retval {}
     *     This is returned if the line could not be folded into
     *     multiple lines.
     */
    std::vector< std::string > SplitLine(
        const std::string& input,
        const std::string& terminator,
        const std::string& continuator,
        StringBreakingStrategy inputSplittingStrategy
    ) {
        std::vector< std::string > output;
        size_t currentLineStart = 0;
        size_t breakOffset = 0;
        while (currentLineStart < input.length()) {
            size_t nextLineStart;
            if (
                !inputSplittingStrategy(
                    input,
                    currentLineStart,
                    breakOffset,
                    nextLineStart
                )
            ) {
                return {};
            }
            std::string part;
            if (!output.empty()) {
                part = continuator;
            }
            part += input.substr(currentLineStart, breakOffset - currentLineStart);
            if (EndsWith(part, terminator)) {
                part += terminator;
            }
            output.push_back(part);
            currentLineStart = nextLineStart;
        }
        return output;
    }

    /**
     * This method takes a substring in a raw internet message
     * corresponding to a single header line, determines where
     * the name and value of the header are, and extracts them.
     *
     * @param[in] rawMessage
     *     This is the string containing the header line.
     *
     * @paraam[in] lineStart
     *     This is the offset into rawMessage where the header
     *     line begins.
     *
     * @param[in] lineEnd
     *     This is the offset into rawMessage where the header
     *     line ends.
     *
     * @param[out] name
     *     This is where to store the extracted header name.
     *
     * @param[out] value
     *     This is where to store the extracted header value.
     *
     * @return
     *     An indication of whether or not the header name and
     *     value were extracted successfully is returned.
     */
    bool SeparateHeaderNameAndValue(
        const std::string& rawMessage,
        size_t lineStart,
        size_t lineEnd,
        MessageHeaders::MessageHeaders::HeaderName& name,
        MessageHeaders::MessageHeaders::HeaderValue& value
    ) {
        auto nameValueDelimiter = rawMessage.find(':', lineStart);
        if (nameValueDelimiter == std::string::npos) {
            return false;
        }
        name = rawMessage.substr(lineStart, nameValueDelimiter - lineStart);
        for (auto c: name) {
            if (IsInvisibleAscii(c)) {
                return false;
            }
        }
        value = rawMessage.substr(
            nameValueDelimiter + 1,
            lineEnd - nameValueDelimiter - 1
        );
        return true;
    }

    /**
     * This function looks ahead in a raw internet message,
     * and for each line that begins with whitespace, it "unfolds"
     * the line and given header value, combining them.
     *
     * The given offset and lineTerminator positions into
     * the raw message are advanced past any unfolded lines.
     *
     * @param[in] rawMessage
     *     This is the string containing the message.
     *
     * @paraam[in] offset
     *     This is the current position into rawMessage where
     *     we should look for lines to potentially unfold
     *     into the given header value.
     *
     * @param[in] lineTerminator
     *     This is the position of the end of the current line.
     *
     * @param[in,out] value
     *     This is the last header value parsed from the message.
     *     If any lines are unfolded, they are added to this value.
     *
     * @return
     *     An indication of whether or not the advancing
     *     and unfolding were successful is returned.
     */
    bool AdvanceAndUnfold(
        const std::string& rawMessage,
        size_t& offset,
        size_t& lineTerminator,
        MessageHeaders::MessageHeaders::HeaderValue& value
    ) {
        for(;;) {
            // Find where the next line begins.
            const auto nextLineStart = lineTerminator + CRLF.length();

            // Find where the next line ends.
            auto nextLineTerminator = rawMessage.find(CRLF, nextLineStart);
            if (nextLineTerminator == std::string::npos) {
                return false;
            }

            // Calculate the next line's length.
            auto nextLineLength = nextLineTerminator - nextLineStart;

            // If the next line begins with whitespace, unfold the line
            if (
                (nextLineLength > CRLF.length())
                && (WSP.find(rawMessage[nextLineStart]) != std::string::npos)
            ) {
                // Append a single space to the header value.
                value += ' ';

                // Remove leading whitespace from the next line.
                const auto firstNonWhitespaceInNextLine = rawMessage.find_first_not_of(WSP, nextLineStart);
                nextLineLength -= (firstNonWhitespaceInNextLine - nextLineStart);

                // Concatenate the rest of the next line to the header value.
                value += rawMessage.substr(firstNonWhitespaceInNextLine, nextLineLength);

                // Move to the line following the next line.
                offset = nextLineTerminator + CRLF.length();
                lineTerminator = nextLineTerminator;
            } else {
                break;
            }
        }
        return true;
    }

}

namespace MessageHeaders {

    MessageHeaders::HeaderName::HeaderName(const std::string& s)
        : name_(s)
    {
    }

    bool MessageHeaders::HeaderName::operator==(const HeaderName& rhs) const noexcept {
        if (name_.length() != rhs.name_.length()) {
            return false;
        }
        for (size_t i = 0; i < name_.length(); ++i) {
            if (tolower(name_[i]) != tolower(rhs.name_[i])) {
                return false;
            }
        }
        return true;
    }

    MessageHeaders::HeaderName::operator const std::string&() const noexcept {
        return name_;
    }

    std::string::const_iterator MessageHeaders::HeaderName::begin() const {
        return name_.begin();
    }

    std::string::const_iterator MessageHeaders::HeaderName::end() const {
        return name_.end();
    }

    MessageHeaders::Header::Header(
        const HeaderName& newName,
        const HeaderValue& newValue
    )
        : name(newName)
        , value(newValue)
    {
    }

    /**
     * This contains the private properties of a MessageHeaders instance.
     */
    struct MessageHeaders::Impl {
        // Properties

        /**
         * These are the headers of the message.
         */
        Headers headers;

        /**
         * This is the maximum number of characters, including
         * the 2-character CRLF line terminator, that should
         * be allowed for a single header line.
         */
        size_t lineLengthLimit = 0;

        /**
         * This indicates whether or not all validity checks
         * have passed for the headers.
         */
        bool valid = true;

        // Methods

        /**
         * This function returns a string splitting strategy
         * function object which can be used once to fold a
         * header line.
         *
         * @return
         *     A string splitting strategy function object
         *     which can be used once to fold a header line is returned.
         */
        StringBreakingStrategy MakeHeaderLineFoldingStrategy() {
            auto firstPart = std::make_shared< bool >(true);
            return [this, firstPart](
                const std::string& s,
                size_t startOffset,
                size_t& breakOffset,
                size_t& nextOffset
            ){
                if (s.length() - startOffset <= lineLengthLimit) {
                    breakOffset = nextOffset = s.length();
                    return true;
                }
                breakOffset = startOffset;
                const auto reservedCharacters = 2 + (*firstPart ? 0 : 1);
                for (size_t i = startOffset; i <= startOffset + lineLengthLimit - reservedCharacters; ++i) {
                    if (WSP.find(s[i]) != std::string::npos) {
                        if (*firstPart) {
                            *firstPart = false;
                        } else {
                            breakOffset = i;
                        }
                    }
                }
                nextOffset = breakOffset + 1;
                return (breakOffset != startOffset);
            };
        }

    };

    MessageHeaders::~MessageHeaders() = default;
    MessageHeaders::MessageHeaders(MessageHeaders&&) = default;
    MessageHeaders& MessageHeaders::operator=(MessageHeaders&&) = default;

    MessageHeaders::MessageHeaders()
        : impl_(new Impl)
    {
    }

    void MessageHeaders::SetLineLimit(size_t newLineLengthLimit) {
        impl_->lineLengthLimit = newLineLengthLimit;
    }

    bool MessageHeaders::IsValid() const {
        return impl_->valid;
    }

    auto MessageHeaders::ParseRawMessage(
        const std::string& rawMessage,
        size_t& bodyOffset
    ) -> State {
        bodyOffset = 0;
        size_t offset = 0;
        while(offset < rawMessage.length()) {
            // Find the end of the current line.
            auto lineTerminator = rawMessage.find(CRLF, offset);
            if (lineTerminator == std::string::npos) {
                if (impl_->lineLengthLimit > 0) {
                    const auto unterminatedLineLength = rawMessage.length() - offset;
                    if (unterminatedLineLength + 2 > impl_->lineLengthLimit) {
                        impl_->valid = false;
                        return State::Error;
                    }
                }
                break;
            }

            // Bail if the line is longer than the limit (if set).
            if (impl_->lineLengthLimit > 0) {
                if (lineTerminator + CRLF.length() - offset > impl_->lineLengthLimit) {
                    impl_->valid = false;
                    return State::Error;
                }
            }

            // Stop if empty line is found -- this is where
            // the headers end and the body (which we don't parse,
            // but leave up to the user to handle) begins.
            if (lineTerminator == offset) {
                offset += CRLF.length();
                break;
            }

            // Separate the header name from the header value.
            HeaderName name;
            HeaderValue value;
            if (
                !SeparateHeaderNameAndValue(
                    rawMessage,
                    offset,
                    lineTerminator,
                    name,
                    value
                )
            ) {
                impl_->valid = false;
            }

            // Look ahead in the raw message and perform
            // line unfolding if we see any lines that begin with whitespace.
            offset = lineTerminator + CRLF.length();
            if (
                !AdvanceAndUnfold(
                    rawMessage,
                    offset,
                    lineTerminator,
                    value
                )
            ) {
                bodyOffset = offset;
                return State::Incomplete;
            }

            // Remove any whitespace that might be at the beginning
            // or end of the header value, and then store the header.
            value = StripMarginWhitespace(value);
            impl_->headers.emplace_back(name, value);
        }
        bodyOffset = offset;
        if (offset == 0) {
            return State::Incomplete;
        }
        return State::Complete;
    }

    auto MessageHeaders::ParseRawMessage(const std::string& rawMessage) -> State {
        size_t bodyOffset;
        return ParseRawMessage(rawMessage, bodyOffset);
    }

    auto MessageHeaders::GetAll() const -> Headers {
        return impl_->headers;
    }

    bool MessageHeaders::HasHeader(const HeaderName& name) const {
        for (const auto& header: impl_->headers) {
            if (header.name == name) {
                return true;
            }
        }
        return false;
    }

    auto MessageHeaders::GetHeaderValue(const HeaderName& name) const -> HeaderValue {
        std::string compositeValue;
        bool isFirstValue = true;
        for (const auto& header: impl_->headers) {
            if (header.name == name) {
                if (isFirstValue) {
                    isFirstValue = false;
                } else {
                    compositeValue += ',';
                }
                compositeValue += header.value;
            }
        }
        return compositeValue;
    }

    auto MessageHeaders::GetHeaderMultiValue(const HeaderName& name) const -> std::vector< HeaderValue > {
        std::vector< HeaderValue > values;
        for (const auto& header: impl_->headers) {
            if (header.name == name) {
                values.push_back(header.value);
            }
        }
        return values;
    }

    auto MessageHeaders::GetHeaderTokens(const HeaderName& name) const -> std::vector< HeaderValue > {
        std::vector< HeaderValue > tokens;
        for (const auto tokenGroup: GetHeaderMultiValue(name)) {
            SplitTokens(tokenGroup, tokens);
        }
        return tokens;
    }

    void MessageHeaders::SetHeader(
        const HeaderName& name,
        const HeaderValue& value
    ) {
        bool haveSetValues = false;
        for (
            auto header = impl_->headers.begin();
            header != impl_->headers.end();
        ) {
            if (header->name == name) {
                if (haveSetValues) {
                    header = impl_->headers.erase(header);
                } else {
                    header->value = value;
                    ++header;
                    haveSetValues = true;
                }
            } else {
                ++header;
            }
        }
        if (!haveSetValues) {
            AddHeader(name, value);
        }
    }

    void MessageHeaders::SetHeader(
        const HeaderName& name,
        const std::vector< HeaderValue >& values,
        bool oneLine
    ) {
        if (values.empty()) {
            return;
        }
        if (oneLine) {
            SetHeader(name, CombineStrings(values, ","));
        } else {
            bool isFirstValue = true;
            for (const auto& value: values) {
                if (isFirstValue) {
                    isFirstValue = false;
                    SetHeader(name, value);
                } else {
                    AddHeader(name, value);
                }
            }
        }
    }

    void MessageHeaders::AddHeader(
        const HeaderName& name,
        const HeaderValue& value
    ) {
        impl_->headers.emplace_back(name, value);
    }

    void MessageHeaders::AddHeader(
        const HeaderName& name,
        const std::vector< HeaderValue >& values,
        bool oneLine
    ) {
        if (values.empty()) {
            return;
        }
        if (oneLine) {
            AddHeader(name, CombineStrings(values, ","));
        } else {
            for (const auto& value: values) {
                AddHeader(name, value);
            }
        }
    }

    void MessageHeaders::RemoveHeader(const HeaderName& name) {
        for (
            auto header = impl_->headers.begin();
            header != impl_->headers.end();
        ) {
            if (header->name == name) {
                header = impl_->headers.erase(header);
            } else {
                ++header;
            }
        }
    }

    std::string MessageHeaders::GenerateRawHeaders() const {
        std::ostringstream rawMessage;
        for (const auto& header: impl_->headers) {
            std::ostringstream lineBuffer;
            lineBuffer << header.name << ": " << header.value << CRLF;
            if (impl_->lineLengthLimit > 0) {
                for (
                    const auto& part: SplitLine(
                        lineBuffer.str(),
                        CRLF,
                        " ",
                        impl_->MakeHeaderLineFoldingStrategy()
                    )
                ) {
                    rawMessage << part;
                }
            } else {
                rawMessage << lineBuffer.str();
            }
        }
        rawMessage << CRLF;
        return rawMessage.str();
    }

    std::ostream& operator<<(
        std::ostream& stream,
        const MessageHeaders::HeaderName& name
    ) {
        return stream << (std::string)name;
    }

    bool operator==(
        const std::string& lhs,
        const MessageHeaders::HeaderName& rhs
    ) {
        return rhs == lhs;
    }

    void PrintTo(
        const MessageHeaders::HeaderName& name,
        std::ostream* os
    ) {
        *os << name;
    }

    void PrintTo(
        const MessageHeaders::State& state,
        std::ostream* os
    ) {
        switch (state) {
            case MessageHeaders::State::Complete: {
                *os << "Complete";
            } break;
            case MessageHeaders::State::Incomplete: {
                *os << "Incomplete";
            } break;
            case MessageHeaders::State::Error: {
                *os << "ERROR";
            } break;
            default: {
                *os << "???";
            };
        }
    }

}
