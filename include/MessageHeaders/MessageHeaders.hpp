#ifndef MESSAGE_HEADERS_HPP
#define MESSAGE_HEADERS_HPP

/**
 * @file MessageHeaders.hpp
 *
 * This module declares the MessageHeaders::MessageHeaders class.
 *
 * © 2018 by Richard Walters
 */

#include <memory>
#include <string>
#include <ostream>
#include <vector>

namespace MessageHeaders {

    /**
     * This class represents the headers of a message on the internet,
     * which is a common theme amongst several internet standards:
     * - e-mail: RFC 5322 (https://tools.ietf.org/html/rfc5322)
     * - HTTP (web): RFC 7230 (https://tools.ietf.org/html/rfc7320)
     * - SIP (VoIP): RFC 3261 (https://tools.ietf.org/html/3261)
     */
    class MessageHeaders {
        // Types
    public:
        /**
         * These are the different states
         * that the message headers can have.
         */
        enum class State {
            /**
             * End of headers not yet found.
             */
            Incomplete,

            /**
             * End of headers found.
             */
            Complete,

            /**
             * Unrecoverable error; reject input.
             */
            Error,
        };

        /**
         * This is how we handle the name of a message header.
         */
        class HeaderName {
            // Lifecycle Management
        public:
            ~HeaderName() noexcept = default;
            HeaderName(const HeaderName& s) = default;
            HeaderName(HeaderName&& s) noexcept = default;
            HeaderName& operator=(const HeaderName&) = default;
            HeaderName& operator=(HeaderName&&) noexcept = default;

            // Public Methods
        public:
            /**
             * This is the default constructor.
             */
            HeaderName() = default;

            /**
             * This constructs the header name based on a normal C++ string.
             *
             * @param[in] s
             *     This is the name to set for the header name.
             */
            HeaderName(const std::string& s);

            /**
             * This constructs the header name based on a normal C string.
             *
             * @param[in] s
             *     This is the name to set for the header name.
             */
            HeaderName(const char* s)
                : name_(s)
            {
            }

            /**
             * This is the equality operator for the class.
             *
             * @param[in] rhs
             *     This is the other header name with which to compare.
             *
             * @return
             *     An indication of whether or not the two header names
             *     are equivalent (case-insensitive) is returned.
             */
            bool operator==(const HeaderName& rhs) const noexcept;

            /**
             * This is the typecast operator to C++ string.
             *
             * @return
             *     The C++ string rendering of the header name is returned.
             */
            operator const std::string&() const noexcept;

            /**
             * This method is used in range-for constructs, to get
             * the beginning iterator of the sequence.  It's merely
             * going to forward to the underlying C++ string holding
             * the name text.
             *
             * @return
             *     The beginning iterator of the sequence is returned.
             */
            std::string::const_iterator begin() const;

            /**
             * This method is used in range-for constructs, to get
             * the ending iterator of the sequence.  It's merely
             * going to forward to the underlying C++ string holding
             * the name text.
             *
             * @return
             *     The ending iterator of the sequence is returned.
             */
            std::string::const_iterator end() const;

            // Private Properties
        private:
            /**
             * This is the content of the header name.
             */
            std::string name_;
        };

        /**
         * This is how we handle the value of a message header.
         */
        typedef std::string HeaderValue;

        /**
         * This represents a single header of the message.
         */
        struct Header {
            // Properties

            /**
             * This is the part of a header that comes before the colon.
             * It identifies the purpose of the header.
             */
            HeaderName name;

            /**
             * This is the part of a header that comes after the colon.
             * It provides the value, setting, or context whose meaning
             * depends on the header name.
             */
            HeaderValue value;

            // Methods

            /**
             * This constructor initializes the header's components.
             *
             * @param[in] newName
             *     This is the part of a header that comes before the colon.
             *     It identifies the purpose of the header.
             *
             * @param[in] newValue
             *     This is the part of a header that comes after the colon.
             *     It provides the value, setting, or context whose meaning
             *     depends on the header name.
             */
            Header(
                const HeaderName& newName,
                const HeaderValue& newValue
            );
        };

        /**
         * This represents the collection of all headers of
         * the message.
         */
        typedef std::vector< Header > Headers;

        // Lifecycle management
    public:
        ~MessageHeaders() noexcept;
        MessageHeaders(const MessageHeaders&) = delete;
        MessageHeaders(MessageHeaders&&) noexcept;
        MessageHeaders& operator=(const MessageHeaders&) = delete;
        MessageHeaders& operator=(MessageHeaders&&) noexcept;

        // Public methods
    public:
        /**
         * This is the default constructor.
         */
        MessageHeaders();

        /**
         * This method sets a limit for the number of characters
         * in any header line.
         *
         * @param[in] newLineLengthLimit
         *     This is the maximum number of characters, including
         *     the 2-character CRLF line terminator, that should
         *     be allowed for a single header line.
         */
        void SetLineLimit(size_t newLineLengthLimit);

        /**
         * This method returns an indication of whether or not
         * the headers constructed so far have all been valid.
         *
         * @return
         *     An indication of whether or not the headers
         *     constructed so far have all been valid is returned.
         */
        bool IsValid() const;

        /**
         * This method determines the headers and body
         * of the message by parsing the raw message from a string.
         *
         * @note
         *     This method does not clear any previous headers
         *     parsed or added to the class instance.  This may
         *     be useful because you can call ParseRawMessage
         *     multiple times to parse a message in fragments.
         *     However, if you're reusing a MessageHeaders, you
         *     may accidentally combine headers from an old message
         *     with a new message, if you don't create a new
         *     message object first.
         *
         * @param[in] rawMessage
         *     This is the string rendering of the message to parse.
         *
         * @param[out] bodyOffset
         *     This is where to store the offset into the given
         *     raw message where the headers ended and the body,
         *     if any, begins.
         *
         * @return
         *     An indication of the state of the parsing of
         *     the headers and the stream from which the headers
         *     were parsed is returned.
         */
        State ParseRawMessage(
            const std::string& rawMessage,
            size_t& bodyOffset
        );

        /**
         * This method determines the headers
         * of the message by parsing the raw message from a string.
         *
         * @note
         *     This method does not clear any previous headers
         *     parsed or added to the class instance.  This may
         *     be useful because you can call ParseRawMessage
         *     multiple times to parse a message in fragments.
         *     However, if you're reusing a MessageHeaders, you
         *     may accidentally combine headers from an old message
         *     with a new message, if you don't create a new
         *     message object first.
         *
         * @param[in] rawMessage
         *     This is the string rendering of the message to parse.
         *
         * @return
         *     An indication of the state of the parsing of
         *     the headers and the stream from which the headers
         *     were parsed is returned.
         */
        State ParseRawMessage(const std::string& rawMessage);

        /**
         * This method returns the collection of headers attached
         * to the message.
         *
         * @return
         *     The collection of headers attached
         *     to the message is returned.
         */
        Headers GetAll() const;

        /**
         * This method checks to see if there is a header in the message
         * with the given name.
         *
         * @param[in] name
         *     This is the name of the header for which to check.
         *
         * @return
         *     An indication of whether or not there is a header
         *     in the message with the given name is returned.
         */
        bool HasHeader(const HeaderName& name) const;

        /**
         * This method returns the value for the header with the given
         * name in the message.
         *
         * @param[in] name
         *     This is the name of the header whose value should be returned.
         *
         * @return
         *     The value of the given header is returned.
         *
         * @note
         *     If there is no header with the given name in the message,
         *     then anything might be returned, but probably just
         *     an empty string.  Good luck with that.
         */
        HeaderValue GetHeaderValue(const HeaderName& name) const;

        /**
         * This method returns the sequence of values for the header
         * with the given name in the message.
         *
         * @param[in] name
         *     This is the name of the header whose values should be returned.
         *
         * @return
         *     The values of the given header are returned.
         *
         * @retval {}
         *     This is returned if there is no header with the given name
         *     in the message.
         */
        std::vector< HeaderValue > GetHeaderMultiValue(const HeaderName& name) const;

        /**
         * This method returns the sequence of tokens extracted from the header
         * with the given name in the message.
         *
         * @param[in] name
         *     This is the name of the header whose values should be returned
         *     as a sequence of tokens.
         *
         * @return
         *     A sequence of tokens extracted from the values of the given
         *     header are returned.
         *
         * @retval {}
         *     This is returned if there is no header with the given name
         *     in the message.
         */
        std::vector< HeaderValue > GetHeaderTokens(const HeaderName& name) const;

        /**
         * This method returns an indication of whether or not the header
         * with the given name in the message contains the given token.
         *
         * @param[in] name
         *     This is the name of the header whose values should be
         *     interpreted as a sequence of tokens, in which to look for
         *     the given token.
         *
         * @return
         *     An indication of whether or not the given header
         *     contains the given token is returned.
         */
        bool HasHeaderToken(
            const HeaderName& name,
            const HeaderValue& token
        ) const;

        /**
         * This method adds or replaces the header with the given name,
         * to have the given value.
         *
         * @param[in] name
         *     This is the name of the header to add or replace.
         *
         * @param[in] value
         *     This is the value of the header to add or replace.
         */
        void SetHeader(
            const HeaderName& name,
            const HeaderValue& value
        );

        /**
         * This method adds or replaces the header with the given name,
         * to have the given values.
         *
         * @param[in] name
         *     This is the name of the header to add or replace.
         *
         * @param[in] values
         *     These are the values of the header to add or replace.
         *
         * @param[in] oneLine
         *     This specifies whether or not to combine the values
         *     into one header line, with values separated by colons.
         */
        void SetHeader(
            const HeaderName& name,
            const std::vector< HeaderValue >& values,
            bool oneLine
        );

        /**
         * This method adds the header with the given name,
         * to have the given value.
         *
         * @param[in] name
         *     This is the name of the header to add.
         *
         * @param[in] value
         *     This is the value of the header to add.
         */
        void AddHeader(
            const HeaderName& name,
            const HeaderValue& value
        );

        /**
         * This method adds the header with the given name,
         * to have the given values.
         *
         * @param[in] name
         *     This is the name of the header to add.
         *
         * @param[in] values
         *     These are the values of the header to add.
         *
         * @param[in] oneLine
         *     This specifies whether or not to combine the values
         *     into one header line, with values separated by colons.
         */
        void AddHeader(
            const HeaderName& name,
            const std::vector< HeaderValue >& values,
            bool oneLine
        );

        /**
         * This method removes the header with the given name
         * from the headers.
         *
         * @param[in] name
         *     This is the name of the header to removes.
         */
        void RemoveHeader(const HeaderName& name);

        /**
         * This method constructs and returns the raw string
         * headers based on the headers that have been
         * collected in the object.
         *
         * @return
         *     The raw string message based on the headers
         *     that have been collected in the object is returned.
         */
        std::string GenerateRawHeaders() const;

        // Private properties
    private:
        /**
         * This is the type of structure that contains the private
         * properties of the instance.  It is defined in the implementation
         * and declared here to ensure that it is scoped inside the class.
         */
        struct Impl;

        /**
         * This contains the private properties of the instance.
         */
        std::unique_ptr< Impl > impl_;
    };

    /**
     * This is a support function for the MessageHeaders::HeaderName class.
     * It's used when header names are shifted out to output streams.
     *
     * @param[in] stream
     *     This is the stream to which we are shifting out the header name.
     *
     * @param[in] name
     *     This is the header name to shift out.
     *
     * @return
     *     The output stream is returned in order to support operation chaining.
     */
    std::ostream& operator<<(
        std::ostream& stream,
        const MessageHeaders::HeaderName& name
    );

    /**
     * This is a support function for the MessageHeaders::HeaderName class.
     * It's used when comparing a string to a header name, and the string
     * is on the left-hand side of the operation.
     *
     * @param[in] lhs
     *     This is the string to compare with the header name.
     *
     * @param[in] rhs
     *     This is the header name to compare with the string.
     *
     * @return
     *     An indication of whether or not the string and header name
     *     are equivalent (case-insensitive) is returned.
     */
    bool operator==(
        const std::string& lhs,
        const MessageHeaders::HeaderName& rhs
    );

    /**
     * This is a support function for Google Test to print out
     * values of the MessageHeaders::HeaderName class.
     *
     * @param[in] name
     *     This is the header name to print.
     *
     * @param[in] os
     *     This points to the stream to which to print the name.
     */
    void PrintTo(
        const MessageHeaders::HeaderName& name,
        std::ostream* os
    );

    /**
     * This is a support function for Google Test to print out
     * values of the MessageHeaders::State class.
     *
     * @param[in] State
     *     This is the message headers state value to print.
     *
     * @param[in] os
     *     This points to the stream to which to print
     *     the message headers state value.
     */
    void PrintTo(
        const MessageHeaders::State& state,
        std::ostream* os
    );

}

#endif /* MESSAGE_HEADERS_HPP */
