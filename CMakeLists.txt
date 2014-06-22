cmake_minimum_required(VERSION 2.8.9)

if(${CMAKE_MAJOR_VERSION} LESS 3)
    project(abuse C CXX)
    set(abuse_VERSION 0.8)
else()
    project(abuse C CXX VERSION 0.8)
endif()

find_package(SDL 1.1.6 REQUIRED)
find_package(SDL_mixer REQUIRED)

include(CheckIncludeFiles)

check_include_files(unistd.h HAVE_UNISTD_H)
check_include_files("sys/time.h" HAVE_SYS_TIME_H)
check_include_files("sys/ioctl.h" HAVE_SYS_IOCTL_H)
check_include_files("netinet/in.h" HAVE_NETINET_IN_H)
check_include_files(bstring.h HAVE_BSTRING_H)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)

add_definitions(-DASSETDIR="/usr/local/abuse")
add_definitions(-DPACKAGE_NAME="abuse")
add_definitions(-DPACKAGE_VERSION="${abuse_VERSION}")
add_definitions(-DHAVE_CONFIG_H)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_subdirectory(src)
