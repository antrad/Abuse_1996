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

#include "common.h"

#include "image.h"
#include "filter.h"

Filter::Filter(int colors)
{
    CONDITION(colors >= 0 && colors <= 256, "bad colors value");
    m_size = colors;
    m_table = (uint8_t *)malloc(m_size);
    memset(m_table, 0, m_size * sizeof(*m_table));
}

// Creates a conversion filter from one palette to another
Filter::Filter(palette *from, palette *to)
{
    m_size = Max(from->pal_size(), to->pal_size());
    m_table = (uint8_t *)malloc(m_size);

    uint8_t *dst = m_table;
    uint8_t *src = (uint8_t *)from->addr();
    int dk = to->darkest(1);

    for (int i = 0; i < m_size; i++)
    {
       int r = *src++;
       int g = *src++;
       int b = *src++;
       int color = to->find_closest(r, g, b);

       // Make sure non-blacks don't get remapped to the transparency
       if ((r || g || b) && to->red(color) == 0
            && to->green(color) == 0 && to->blue(color) == 0)
           color = dk;

       *dst++ = color;
    }
}

Filter::~Filter()
{
    free(m_table);
}

void Filter::Set(int color_num, int change_to)
{
    CONDITION(color_num >= 0 && color_num < m_size, "Bad colors_num");
    m_table[color_num] = change_to;
}

void Filter::Apply(image *im)
{
    im->Lock();
    uint8_t *dst = im->scan_line(0);
    int npixels = im->Size().x * im->Size().y;
    while (npixels--)
    {
        CONDITION(*dst < m_size, "not enough filter colors");
        *dst = m_table[*dst];
        dst++;
    }
    im->Unlock();
}

/* This is only ever used in the editor, when showing the toolbar. It
 * does not look like it's very useful. */
void Filter::PutImage(image *screen, image *im, ivec2 pos)
{
    ivec2 aa = ivec2(0), bb = im->Size(), caa, cbb;
    screen->GetClip(caa, cbb);

    // See if the image gets clipped off the screen
    if (!(pos < cbb && pos + (bb - aa) > caa))
        return;

    aa += Max(caa - pos, 0);
    pos = Max(pos, caa);
    bb = Min(bb, cbb - pos + aa);

    if (!(aa < bb))
        return;

    ivec2 span = bb - aa;

    screen->AddDirty(pos, pos + span);

    screen->Lock();
    im->Lock();

    for (int j = 0; j < span.y; j++)
    {
        uint8_t *src = im->scan_line(aa.y + j) + aa.x;
        uint8_t *dst = screen->scan_line(pos.y + j) + pos.x;

        for (int i = 0; i < span.x; i++, src++, dst++)
            if (*src)
                *dst = m_table[*src];
    }

    im->Unlock();
    screen->Unlock();
}

ColorFilter::ColorFilter(palette *pal, int color_bits)
{
    int max = pal->pal_size();
    int mul = 1 << (8 - color_bits);
    m_size = 1 << color_bits;
    m_table = (uint8_t *)malloc(m_size * m_size * m_size);

    /* For each colour in the RGB cube, find the nearest palette element. */
    for (int r = 0; r < m_size; r++)
    for (int g = 0; g < m_size; g++)
    for (int b = 0; b < m_size; b++)
    {
        int best = 256 * 256 * 3;
        int color = 0;
        uint8_t *pp = (uint8_t *)pal->addr();

        for (int i = 0; i < max; i++)
        {
            int rd = *pp++ - r * mul,
                gd = *pp++ - g * mul,
                bd = *pp++ - b * mul;

            int dist = rd * rd + bd * bd + gd * gd;
            if (dist < best)
            {
                best = dist;
                color = i;
            }
        }
        m_table[(r * m_size + g) * m_size + b] = color;
    }
}

ColorFilter::ColorFilter(spec_entry *e, bFILE *fp)
{
    fp->seek(e->offset, 0);
    m_size = fp->read_uint16();
    m_table = (uint8_t *)malloc(m_size * m_size * m_size);
    fp->read(m_table, m_size * m_size * m_size);
}

ColorFilter::~ColorFilter()
{
    free(m_table);
}

size_t ColorFilter::DiskUsage()
{
    return sizeof(uint16_t) + m_size * m_size * m_size;
}

int ColorFilter::Write(bFILE *fp)
{
    fp->write_uint16(m_size);
    int bytes = m_size * m_size * m_size;
    return fp->write(m_table, bytes) == bytes;
}

