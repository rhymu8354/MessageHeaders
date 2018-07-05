# CMakeLists.txt for InternetMessage
#
# © 2018 by Richard Walters

cmake_minimum_required(VERSION 3.8)
set(This InternetMessage)

set(Headers
    include/InternetMessage/InternetMessage.hpp
)

set(Sources
    src/InternetMessage.cpp
)

add_library(${This} STATIC ${Sources} ${Headers})
set_target_properties(${This} PROPERTIES
    FOLDER Libraries
)

target_include_directories(${This} PUBLIC include)

add_subdirectory(test)
