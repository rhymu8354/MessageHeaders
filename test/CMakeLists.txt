# CMakeLists.txt for MessageHeadersTests
#
# © 2018 by Richard Walters

cmake_minimum_required(VERSION 3.8)
set(This MessageHeadersTests)

set(Sources
    src/MessageHeadersTests.cpp
)

add_executable(${This} ${Sources})
set_target_properties(${This} PROPERTIES
    FOLDER Tests
)

target_include_directories(${This} PRIVATE ..)

target_link_libraries(${This} PUBLIC
    gtest_main
    MessageHeaders
)

add_test(
    NAME ${This}
    COMMAND ${This}
)
