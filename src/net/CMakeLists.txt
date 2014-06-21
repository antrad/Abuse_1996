add_library(net
    gserver.cpp gserver.h
    gclient.cpp gclient.h
    fileman.cpp fileman.h
    sock.cpp sock.h
    tcpip.cpp tcpip.h
    ghandler.h undrv.h
)

include_directories(
    ${abuse_SOURCE_DIR}/src
    ${abuse_SOURCE_DIR}/src/imlib
    ${SDL_INCLUDE_DIR}
    ${SDL_MIXER_INCLUDE_DIR}
)

#CPPFLAGS = @CPPFLAGS@ -DNO_CHECK
