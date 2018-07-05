#ifndef INTERNET_MESSAGE_HPP
#define INTERNET_MESSAGE_HPP

/**
 * @file InternetMessage.hpp
 *
 * This module declares the InternetMessage::InternetMessage class.
 *
 * © 2018 by Richard Walters
 */

#include <memory>
#include <string>
#include <vector>

namespace InternetMessage {

    /**
     * This class represents an InternetMessage,
     * as defined in RFC 2822 (https://tools.ietf.org/html/rfc2822).
     */
    class InternetMessage {
        // Types
    public:
        /**
         * This is how we handle the name of a internet message header.
         */
        typedef std::string HeaderName;

        /**
         * This is how we handle the value of a internet message header.
         */
        typedef std::string HeaderValue;

        /**
         * This represents a single header of the internet message.
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
         * the internet message.
         */
        typedef std::vector< Header > Headers;

        // Lifecycle management
    public:
        ~InternetMessage();
        InternetMessage(const InternetMessage&) = delete;
        InternetMessage(InternetMessage&&) = delete;
        InternetMessage& operator=(const InternetMessage&) = delete;
        InternetMessage& operator=(InternetMessage&&) = delete;

        // Public methods
    public:
        /**
         * This is the default constructor.
         */
        InternetMessage();

        /**
         * This method determines the headers and body
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
        Headers GetHeaders() const;

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
         * This method returns the part of the message that follows
         * all the headers, and represents the principal content
         * of the overall message.
         *
         * @return
         *     The body of the message is returned.
         */
        std::string GetBody() const;

        /**
         * This method constructs and returns the raw string
         * internet message based on the headers and body that have been
         * collected in the object.
         *
         * @return
         *     The raw string internet message based on the headers and body
         *     that have been collected in the object is returned.
         */
        std::string GenerateRawMessage() const;

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

#endif /* INTERNET_MESSAGE_HPP */
