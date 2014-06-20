/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef VIS_OBJECT_HPP
#define VIS_OBJECT_HPP

#include "jwindow.h"
#include "filter.h"

class visual_object
{
public:
    virtual void draw(image *screen, int x, int y, Filter *f) = 0;
    virtual int width() = 0;
    virtual int height() = 0;
    virtual ~visual_object() { }
};

#endif

