
DEFS = @DEFS@

noinst_LIBRARIES = libsdlport.a

libsdlport_a_SOURCES = \
    video.cpp \
    mouse.cpp \
    event.cpp \
    sound.cpp sound.h \
    timing.cpp \
    jdir.cpp \
    joystick.cpp joy.h \
    setup.cpp setup.h \
    hmi.cpp hmi.h \
    $(NULL)
libsdlport_a_LIBADD =

INCLUDES = -I$(srcdir)/../ -I$(srcdir)/../imlib $(SDL_CFLAGS)

CPPFLAGS = @CPPFLAGS@ -DNO_CHECK -DASSETDIR=\"$(assetdir)\"

