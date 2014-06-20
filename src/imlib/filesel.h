/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __FSELECT_HPP_
#define __FSELECT_HPP_

#include "jwindow.h"

Jwindow *file_dialog(char const *prompt, char const *def,
             int ok_id, char const *ok_name, int cancel_id,
                     char const *cancel_name, char const *FILENAME_str,
             int filename_id);

#endif




