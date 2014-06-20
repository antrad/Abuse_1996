/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __PCX_READ_HPP__
#define __PCX_READ_HPP__

#include "image.h"
#include "palette.h"

void write_PCX(image *im, palette *pal, char const *filename);
image *read_PCX(char const *filename, palette *&pal);

#endif
