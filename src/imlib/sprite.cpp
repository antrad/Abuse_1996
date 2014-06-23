/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#if defined HAVE_CONFIG_H
#   include "config.h"
#endif

#include <math.h>

#include "common.h"

#include "video.h"
#include "image.h"
#include "palette.h"
#include "linked.h"
#include "sprite.h"

Sprite::Sprite(image *screen, image *visual, ivec2 pos)
{
    CHECK(visual && screen);
    m_pos = pos;
    m_visual = visual;
    m_screen = screen;
    m_save = new image(visual->Size());

    if (m_pos + visual->Size() >= 0 && m_pos < ivec2(xres, yres))
        m_save->PutPart(m_screen, ivec2(0,0), m_pos, m_pos + m_save->Size());
}

Sprite::~Sprite()
{
    delete m_save;
}

void Sprite::SetVisual(image *visual, int delete_old)
{
    if (delete_old)
        delete m_visual;
    m_visual = visual;
    if (m_save->Size() != visual->Size())
    {
        delete m_save;
        m_save = new image(visual->Size());
    }

    if (m_pos + visual->Size() >= 0 && m_pos < ivec2(xres, yres))
        m_save->PutPart(m_screen, ivec2(0,0), m_pos, m_pos + m_save->Size());
}

