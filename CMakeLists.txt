cmake_minimum_required(VERSION 3.0)

project(abuse VERSION 0.8)

find_package(SDL 1.1.6 REQUIRED)
find_package(SDL_mixer REQUIRED)

add_definitions(-DPACKAGE_NAME="abuse")
add_definitions(-DPACKAGE_VERSION="${abuse_VERSION}")
add_definitions(-DASSETDIR="/usr/local/abuse")

add_subdirectory(src)
