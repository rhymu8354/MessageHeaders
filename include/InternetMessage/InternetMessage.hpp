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

namespace InternetMessage {

    /**
     * This class represents an InternetMessage,
     * as defined in RFC 2822 (https://tools.ietf.org/html/rfc2822).
     */
    class InternetMessage {
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
