## Process this file with automake to produce Makefile.in

SUBDIRS = lisp net imlib sdlport

bin_PROGRAMS = abuse abuse-tool

abuse_SOURCES = \
    common.h \
    \
    lol/matrix.cpp lol/matrix.h \
    lol/timer.cpp lol/timer.h \
    \
    specache.cpp specache.h \
    netcfg.cpp netcfg.h \
    innet.cpp \
    chat.cpp chat.h \
    endgame.cpp \
    loadgame.cpp loadgame.h \
    profile.cpp profile.h \
    cop.cpp cop.h \
    statbar.cpp \
    compiled.cpp compiled.h \
    ant.cpp ant.h \
    sensor.cpp \
    demo.cpp demo.h \
    lcache.cpp lcache.h \
    nfclient.cpp nfclient.h \
    clisp.cpp clisp.h \
    gui.cpp gui.h \
    transp.cpp transp.h \
    collide.cpp \
    property.cpp property.h \
    cache.cpp cache.h \
    particle.cpp particle.h \
    objects.cpp objects.h \
    extend.cpp extend.h \
    console.cpp console.h \
    ability.cpp ability.h \
    items.cpp items.h \
    dev.cpp dev.h \
    chars.cpp chars.h \
    level.cpp level.h \
    smallfnt.cpp \
    automap.cpp automap.h \
    help.cpp help.h \
    intsect.cpp intsect.h \
    loader2.cpp loader2.h \
    seq.cpp seq.h \
    points.cpp points.h \
    fnt6x13.cpp \
    morpher.cpp morpher.h \
    menu.cpp menu.h \
    director.cpp director.h \
    view.cpp view.h \
    configuration.cpp configuration.h \
    game.cpp game.h \
    light.cpp light.h \
    devsel.cpp devsel.h \
    crc.cpp crc.h \
    gamma.cpp gamma.h \
    id.h netface.h isllist.h sbar.h \
    nfserver.h \
    \
    ui/volumewindow.cpp ui/volumewindow.h \
    $(NULL)

EXTRALIBS = lisp/liblisp.a sdlport/libsdlport.a imlib/libimlib.a net/libnet.a
abuse_LDADD = $(EXTRALIBS)
abuse_DEPENDENCIES = $(EXTRALIBS)

abuse_tool_SOURCES = \
    tool/abuse-tool.cpp \
    crc.cpp crc.h
abuse_tool_LDADD = imlib/libimlib.a
abuse_tool_DEPENDENCIES = $(abuse_tool_LDADD)

INCLUDES = -I$(srcdir) -I $(srcdir)/lisp -I$(srcdir)/imlib \
           -I$(srcdir)/net $(SDL_CFLAGS)

CPPFLAGS = @CPPFLAGS@ -DNO_CHECK
