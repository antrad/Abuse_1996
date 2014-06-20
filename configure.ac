dnl Process this file with autoconf to produce a configure script.
AC_INIT(abuse, 0.8)
AC_CONFIG_AUX_DIR(.auto)
AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(config.h)

dnl Checks for programs.
AC_PROG_LIBTOOL
AC_ISC_POSIX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_CXX
AC_PROG_INSTALL
AC_C_BIGENDIAN

dnl Check for X
AC_PATH_X
if [ "x$x_libraries" != "x" ]; then
  X_LIBS="-L$x_libraries"
fi

AC_ARG_ENABLE(debug,
  [  --enable-debug          build debug versions of the game (default no)])
AC_ARG_ENABLE(release,
  [  --enable-release        build final release of the game (default no)])
AC_ARG_ENABLE(nonfree,
  [  --disable-nonfree       disable non-free data (default no)])

AC_ARG_WITH(assetdir,
  [  --with-assetdir=DIR     override default game data directory])

dnl  Figure out where the datafiles will be
if test "x${with_assetdir}" = x; then
  with_assetdir='$(datadir)/games/abuse'
fi
AC_SUBST([assetdir], [$with_assetdir])

dnl  Checks for libraries
dnl Do we need to link against something for X shared memory support?
AC_CHECK_LIB(Xext, XShmAttach, :,
             [AC_CHECK_LIB(XextSam, XShmAttach, LIBS="$LIBS -lXextSam", :,
                           $X_LIBS -lX11 -lXext)],
             $X_LIBS -lX11)

dnl Checks for Solaris compatibility
AC_CHECK_LIB(m, pow, LIBS="$LIBS -lm")
AC_CHECK_LIB(socket, socket, LIBS="$LIBS -lsocket")
AC_CHECK_LIB(nsl, gethostbyname, LIBS="$LIBS -lnsl")

dnl Check for SDL
SDL_VERSION=1.1.6
AM_PATH_SDL($SDL_VERSION, :,
    AC_MSG_ERROR([*** SDL version $SDL_VERSION or above not found!]))
CFLAGS="$CFLAGS $SDL_CFLAGS"
LIBS="$LIBS $SDL_LIBS $X_LIBS"

dnl Check for SDL mixer
ac_cv_my_have_sdl_mixer="no"
save_CPPFLAGS="${CPPFLAGS}"
CPPFLAGS="${CPPFLAGS} ${SDL_CFLAGS}"
AC_CHECK_HEADERS(SDL/SDL_mixer.h, [ac_cv_my_have_sdl_mixer="yes"])
CPPFLAGS="${save_CPPFLAGS}"
if test "${ac_cv_my_have_sdl_mixer}" = "no"; then
  AC_MSG_ERROR([*** SDL_mixer not found!])
fi
LIBS="$LIBS -lSDL_mixer"

if test "${enable_debug}" = "yes"; then
  AC_DEFINE(HAVE_DEBUG, 1, Define to 1 to activate debug)
  OPT="-O0"
else
  OPT="-O2 -fno-strength-reduce -fomit-frame-pointer"
fi

if test "${enable_release}" = "yes"; then
  AC_DEFINE(HAVE_RELEASE, 1, Define to 1 to activate final release)
  REL=""
else
  REL="-g"
fi

dnl  Is our package stripped from its non-free data? Or did the user
dnl  maybe disable non-free data?
ac_cv_have_nonfree="no"
if test -f "${srcdir}/data/sfx/ambcave1.wav"; then
  ac_cv_have_nonfree="yes"
fi
if test "${enable_nonfree}" = "no"; then
  ac_cv_have_nonfree="no"
fi
AM_CONDITIONAL(HAVE_NONFREE, test "${ac_cv_have_nonfree}" = "yes")

# Optimizations
CXXFLAGS="${CXXFLAGS} ${OPT} ${REL}"
# Code qui fait des warnings == code de porc == deux baffes dans ta gueule
CXXFLAGS="${CXXFLAGS} -Wall -Wpointer-arith -Wcast-align -Wcast-qual -Wsign-compare"

dnl Checks for header files
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_CHECK_HEADERS(fcntl.h malloc.h string.h sys/ioctl.h sys/time.h unistd.h)
AC_CHECK_HEADERS(netinet/in.h)

dnl Checks for functions
AC_FUNC_MEMCMP
AC_CHECK_FUNCS(atexit on_exit strstr gettimeofday)

dnl Check for OpenGL
dnl Should this be more thorough?
dnl For OpenGL support on OSX it's better to use Project Builder as -lGL
dnl doesn't seem to work this way.
AC_MSG_CHECKING(for OpenGL support)
have_opengl=no
AC_TRY_COMPILE([
    #ifdef WIN32
    #include <windows.h>
    #elif defined(__APPLE__) && defined(__MACH__)
/*    #include <OpenGL/gl.h>*/
    #error	/* Error so the compile fails on OSX */
    #else
    #include <GL/gl.h>
    #endif],
  [],
  [have_opengl=yes])
AC_MSG_RESULT($have_opengl)
if test "x$have_opengl" = xyes; then
    CFLAGS="$CFLAGS -DHAVE_OPENGL"
    CXXFLAGS="$CXXFLAGS -DHAVE_OPENGL"
    LIBS="$LIBS -lGL -lpthread"
fi

AC_OUTPUT([
    Makefile
    src/Makefile
    src/lisp/Makefile
    src/net/Makefile
    src/imlib/Makefile
    src/sdlport/Makefile
    data/Makefile
    doc/Makefile
    doc/abuse.6
    doc/abuse-tool.6])

echo "
------ Configuration for $PACKAGE $VERSION ------
    Compiler:          ${CC} (`${CC} --version | head -n 1`)
    Install prefix:    ${prefix}
    Asset directory:   ${with_assetdir}
    SDL version:       `sdl-config --version`
    OpenGL support:    ${have_opengl}

Now type 'make' to build $PACKAGE.
"
