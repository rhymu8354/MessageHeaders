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
         * This is how we handle the name of a message header.
         */
        typedef std::string HeaderName;

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
        ~MessageHeaders();
        MessageHeaders(const MessageHeaders&) = delete;
        MessageHeaders(MessageHeaders&&) = delete;
        MessageHeaders& operator=(const MessageHeaders&) = delete;
        MessageHeaders& operator=(MessageHeaders&&) = delete;

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
         * This method determines the headers and body
         * of the message by parsing the raw message from a string.
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
         *     An indication of whether or not the message was
         *     parsed successfully is returned.
         */
        bool ParseRawMessage(
            const std::string& rawMessage,
            size_t& bodyOffset
        );

        /**
         * This method determines the headers
         * of the message by parsing the raw message from a string.
         *
         * @param[in] rawMessage
         *     This is the string rendering of the message to parse.
         *
         * @return
         *     An indication of whether or not the message was
         *     parsed successfully is returned.
         */
        bool ParseRawMessage(const std::string& rawMessage);

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
         * This method add or replace the header with the given name,
         * to have the given.
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
        std::unique_ptr< struct Impl > impl_;
    };

}

#endif /* MESSAGE_HEADERS_HPP */
