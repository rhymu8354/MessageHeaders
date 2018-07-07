# CMakeLists.txt for MessageHeaders
#
# © 2018 by Richard Walters

cmake_minimum_required(VERSION 3.8)
set(This MessageHeaders)

set(Headers
    include/MessageHeaders/MessageHeaders.hpp
)

set(Sources
    src/MessageHeaders.cpp
)

add_library(${This} STATIC ${Sources} ${Headers})
set_target_properties(${This} PROPERTIES
    FOLDER Libraries
)

target_include_directories(${This} PUBLIC include)

add_subdirectory(test)
