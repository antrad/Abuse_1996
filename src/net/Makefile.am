## Process this file with automake to produce Makefile.in
noinst_LIBRARIES = libnet.a

libnet_a_SOURCES = \
    gserver.cpp gserver.h \
    gclient.cpp gclient.h \
    fileman.cpp fileman.h \
    sock.cpp sock.h \
    tcpip.cpp tcpip.h \
    ghandler.h undrv.h \
    $(NULL)

INCLUDES = -I$(srcdir) -I$(srcdir)/../ -I$(srcdir)/../imlib $(SDL_CFLAGS)

CPPFLAGS = @CPPFLAGS@ -DNO_CHECK
