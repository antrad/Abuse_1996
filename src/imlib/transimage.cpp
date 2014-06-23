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

#include <cstdio>
#include <cstring>

#include "common.h"

#include "transimage.h"

TransImage::TransImage(image *im, char const *name)
{
    m_size = im->Size();

    im->Lock();

    // First find out how much data to allocate
    size_t bytes = 0;
    for (int y = 0; y < m_size.y; y++)
    {
        uint8_t *parser = im->scan_line(y);
        for (int x = 0; x < m_size.x; )
        {
            bytes++;
            while (x < m_size.x && *parser == 0)
            {
                parser++; x++;
            }

            if (x >= m_size.x)
                break;

            bytes++;  // byte for the size of the run
            while (x < m_size.x && *parser != 0)
            {
                bytes++;
                x++;
                parser++;
            }
        }
    }

    uint8_t *parser = m_data = (uint8_t *)malloc(bytes);
    if (!parser)
    {
        printf("size = %d %d (%ld bytes)\n", m_size.x, m_size.y, (long)bytes);
        CONDITION(parser, "malloc error for TransImage::m_data");
    }

    // Now fill the RLE transparency image
    for (int y = 0; y < m_size.y; y++)
    {
        uint8_t *sl = im->scan_line(y);

        for (int x = 0; x < m_size.x; )
        {
            uint8_t len = 0;
            while (x + len < m_size.x && sl[len] == 0)
                len++;

            *parser++ = len;
            x += len;
            sl += len;

            if (x >= m_size.x)
                break;

            len = 0;
            while (x + len < m_size.x && sl[len] != 0)
            {
                parser[len + 1] = sl[len];
                len++;
            }

            *parser++ = len;
            parser += len;
            x += len;
            sl += len;
        }
    }
    im->Unlock();
}

TransImage::~TransImage()
{
    free(m_data);
}

image *TransImage::ToImage()
{
    image *im = new image(m_size);

    // FIXME: this is required until FILLED mode is fixed
    im->Lock();
    memset(im->scan_line(0), 0, m_size.x * m_size.y);
    im->Unlock();

    PutImage(im, ivec2(0));
    return im;
}

uint8_t *TransImage::ClipToLine(image *screen, ivec2 pos1, ivec2 pos2,
                                ivec2 &pos, int &ysteps)
{
    // check to see if it is totally clipped out first
    if (pos.y + m_size.y <= pos1.y || pos.y >= pos2.y
         || pos.x >= pos2.x || pos.x + m_size.x <= pos1.x)
        return NULL;

    uint8_t *parser = m_data;

    // Number of lines to skip, number of lines to draw, first line to draw
    int skiplines = Max(pos1.y - pos.y, 0);
    ysteps = Min(pos2.y - pos.y, m_size.y - skiplines);
    pos.y += skiplines;

    while (skiplines--)
    {
        for (int ix = 0; ix < m_size.x; )
        {
            ix += *parser++; // skip over empty space

            if (ix >= m_size.x)
                break;

            ix += *parser;
            parser += *parser + 1; // skip over data
        }
    }

    screen->AddDirty(ivec2(Max(pos.x, pos1.x), pos.y),
                     ivec2(Min(pos.x + m_size.x, pos2.x), pos.y + m_size.y));
    return parser;
}

template<int N>
void TransImage::PutImageGeneric(image *screen, ivec2 pos, uint8_t color,
                                 image *blend, ivec2 bpos, uint8_t *map,
                                 uint8_t *map2, int amount, int nframes,
                                 uint8_t *tint, ColorFilter *f, palette *pal)
{
    ivec2 pos1, pos2;
    int ysteps, mul = 0;

    screen->GetClip(pos1, pos2);

    if (N == SCANLINE)
    {
        pos1.y = Max(pos1.y, pos.y + amount);
        pos2.y = Min(pos2.y, pos.y + amount + 1);
        if (pos1.y >= pos2.y)
            return;
    }

    uint8_t *datap = ClipToLine(screen, pos1, pos2, pos, ysteps),
            *screen_line, *blend_line = NULL, *paddr = NULL;
    if (!datap)
        return; // if ClipToLine says nothing to draw, return

    CONDITION(N != BLEND || (pos.y >= bpos.y
                              && pos.y + ysteps <= bpos.y + blend->Size().y),
              "Blend doesn't fit on TransImage");

    if (N == FADE || N == FADE_TINT || N == BLEND)
        paddr = (uint8_t *)pal->addr();

    if (N == FADE || N == FADE_TINT)
        mul = (amount << 16) / nframes;
    else if (N == BLEND)
        mul = ((16 - amount) << 16 / 16);

    if (N == PREDATOR)
        ysteps = Min(ysteps, pos2.y - 1 - pos.y - 2);

    screen->Lock();

    screen_line = screen->scan_line(pos.y) + pos.x;
    int sw = screen->Size().x;
    pos1.x -= pos.x; pos2.x -= pos.x;

    for (; ysteps > 0; ysteps--, pos.y++)
    {
        if (N == BLEND)
            blend_line = blend->scan_line(pos.y - bpos.y);

        for (int ix = 0; ix < m_size.x; )
        {
            // Handle a run of transparent pixels
            int todo = *datap++;

            // FIXME: implement FILLED mode
            ix += todo;
            screen_line += todo;

            if (ix >= m_size.x)
                break;

            // Handle a run of solid pixels
            todo = *datap++;

            // Chop left side if necessary, but no more than todo
            int tochop = Min(todo, Max(pos1.x - ix, 0));

            ix += tochop;
            screen_line += tochop;
            datap += tochop;
            todo -= tochop;

            // Chop right side if necessary and process the remaining pixels
            int count = Min(todo, Max(pos2.x - ix, 0));

            if (N == NORMAL || N == SCANLINE)
            {
                memcpy(screen_line, datap, count);
            }
            else if (N == COLOR)
            {
                memset(screen_line, color, count);
            }
            else if (N == PREDATOR)
            {
                memcpy(screen_line, screen_line + 2 * m_size.x, count);
            }
            else if (N == REMAP)
            {
                uint8_t *sl = screen_line, *sl2 = datap;
                while (count--)
                    *sl++ = map[*sl2++];
            }
            else if (N == REMAP2)
            {
                uint8_t *sl = screen_line, *sl2 = datap;
                while (count--)
                    *sl++ = map2[map[*sl2++]];
            }
            else if (N == FADE || N == FADE_TINT || N == BLEND)
            {
                uint8_t *sl = screen_line;
                uint8_t *sl2 = (N == BLEND) ? blend_line + pos.x + ix - bpos.x
                                            : sl;
                uint8_t *sl3 = datap;

                while (count--)
                {
                    uint8_t *p1 = paddr + 3 * *sl2++;
                    uint8_t *p2 = paddr + 3 * (N == FADE_TINT ? tint[*sl3++]
                                                              : *sl3++);

                    uint8_t r = ((((int)p1[0] - p2[0]) * mul) >> 16) + p2[0];
                    uint8_t g = ((((int)p1[1] - p2[1]) * mul) >> 16) + p2[1];
                    uint8_t b = ((((int)p1[2] - p2[2]) * mul) >> 16) + p2[2];

                    *sl++ = f->Lookup(r >> 3, g >> 3, b >> 3);
                }
            }

            datap += todo;
            ix += todo;
            screen_line += todo;
        }
        screen_line += sw - m_size.x;
    }
    screen->Unlock();
}

void TransImage::PutImage(image *screen, ivec2 pos)
{
    PutImageGeneric<NORMAL>(screen, pos, 0, NULL, 0, NULL, NULL,
                            0, 1, NULL, NULL, NULL);
}

void TransImage::PutRemap(image *screen, ivec2 pos, uint8_t *map)
{
    PutImageGeneric<REMAP>(screen, pos, 0, NULL, 0, map, NULL,
                           0, 1, NULL, NULL, NULL);
}

void TransImage::PutDoubleRemap(image *screen, ivec2 pos,
                            uint8_t *map, uint8_t *map2)
{
    PutImageGeneric<REMAP2>(screen, pos, 0, NULL, 0, map, map2,
                            0, 1, NULL, NULL, NULL);
}

// Used when eg. the player teleports, or in rocket trails
void TransImage::PutFade(image *screen, ivec2 pos, int amount, int nframes,
                         ColorFilter *f, palette *pal)
{
    PutImageGeneric<FADE>(screen, pos, 0, NULL, 0, NULL, NULL,
                          amount, nframes, NULL, f, pal);
}

void TransImage::PutFadeTint(image *screen, ivec2 pos, int amount, int nframes,
                             uint8_t *tint, ColorFilter *f, palette *pal)
{
    PutImageGeneric<FADE_TINT>(screen, pos, 0, NULL, 0, NULL, NULL,
                               amount, nframes, tint, f, pal);
}

void TransImage::PutColor(image *screen, ivec2 pos, uint8_t color)
{
    PutImageGeneric<COLOR>(screen, pos, color, NULL, 0, NULL, NULL,
                           0, 1, NULL, NULL, NULL);
}

// This method is unused but is believed to work.
// Assumes that the blend image completely covers the transparent image.
void TransImage::PutBlend(image *screen, ivec2 pos, image *blend, ivec2 bpos,
                          int amount, ColorFilter *f, palette *pal)
{
    PutImageGeneric<BLEND>(screen, pos, 0, blend, bpos, NULL, NULL,
                           amount, 1, NULL, f, pal);
}

void TransImage::PutFilled(image *screen, ivec2 pos, uint8_t color)
{
    PutImageGeneric<FILLED>(screen, pos, color, NULL, 0, NULL, NULL,
                            0, 1, NULL, NULL, NULL);
}

void TransImage::PutPredator(image *screen, ivec2 pos)
{
    PutImageGeneric<PREDATOR>(screen, pos, 0, NULL, 0, NULL, NULL,
                              0, 1, NULL, NULL, NULL);
}

void TransImage::PutScanLine(image *screen, ivec2 pos, int line)
{
    PutImageGeneric<SCANLINE>(screen, pos, 0, NULL, 0, NULL, NULL,
                              line, 1, NULL, NULL, NULL);
}

size_t TransImage::DiskUsage()
{
    uint8_t *d = m_data;
    size_t ret = 0;

    for (int y = 0; y < m_size.y; y++)
    {
        for (int x = 0; x < m_size.x; x++)
        {
            x += *d++; ret++;

            if (x >= m_size.x)
                break;

            size_t run = *d++; ret += run + 1; d += run; x += run;
        }
    }
    return ret + sizeof(void *) + sizeof(ivec2);
}

