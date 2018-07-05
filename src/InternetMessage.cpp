/**
 * @file InternetMessage.cpp
 *
 * This module contains the implementation of the InternetMessage::InternetMessage class.
 *
 * © 2018 by Richard Walters
 */

#include <InternetMessage/InternetMessage.hpp>

namespace InternetMessage {

    /**
     * This contains the private properties of a InternetMessage instance.
     */
    struct InternetMessage::Impl {
    };

    InternetMessage::~InternetMessage() = default;

    InternetMessage::InternetMessage()
        : impl_(new Impl)
    {
    }

}
