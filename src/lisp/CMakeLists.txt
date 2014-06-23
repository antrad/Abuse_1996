add_library(lisp
    lisp.cpp lisp.h
    lisp_opt.cpp lisp_opt.h
    lisp_gc.cpp lisp_gc.h
    trig.cpp
    stack.h symbols.h
)

include_directories(
    ${abuse_SOURCE_DIR}/src
    ${abuse_SOURCE_DIR}/src/imlib
    ${SDL_INCLUDE_DIR}
    ${SDL_MIXER_INCLUDE_DIR}
)
#INCLUDES = -I$(srcdir) -I$(srcdir)/../ -I$(srcdir)/../imlib $(SDL_CFLAGS)

#CPPFLAGS = @CPPFLAGS@ -DNO_CHECK
