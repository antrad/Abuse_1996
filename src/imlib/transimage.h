/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __TIMAGE_HPP__
#define __TIMAGE_HPP__

#include "image.h"
#include "palette.h"
#include "filter.h"

/*  Data is stored in the following format:
 *
 *   uint8_t skip;       // transparent pixel count
 *   uint8_t size;       // solid pixel count
 *   uint8_t data[size]; // solid pixel values
 *   ...
 *   (no scan line wraps allowed, there can be a last skip value)
 */

class TransImage
{
public:
    TransImage(image *im, char const *name);
    ~TransImage();

    inline vec2i Size() { return m_size; }
    inline uint8_t *Data() { return m_data; }

    image *ToImage();

    void PutImage(image *screen, vec2i pos);
    void PutRemap(image *screen, vec2i pos, uint8_t *map);
    void PutDoubleRemap(image *screen, vec2i pos, uint8_t *map, uint8_t *map2);
    void PutFade(image *screen, vec2i pos, int amount, int nframes,
                 ColorFilter *f, palette *pal);
    void PutFadeTint(image *screen, vec2i pos, int amount, int nframes,
                     uint8_t *tint, ColorFilter *f, palette *pal);
    void PutColor(image *screen, vec2i pos, uint8_t color);
    void PutFilled(image *screen, vec2i pos, uint8_t color);
    void PutPredator(image *screen, vec2i pos);
    void PutBlend(image *screen, vec2i pos, image *blend, vec2i bpos,
                  int blend_amount, ColorFilter *f, palette *pal);
    void PutScanLine(image *screen, vec2i pos, int line);

    size_t DiskUsage();

private:
    uint8_t *ClipToLine(image *screen, vec2i pos1, vec2i pos2,
                        vec2i &posy, int &ysteps);

    enum PutMode { NORMAL, REMAP, REMAP2, FADE, FADE_TINT, COLOR,
                   FILLED, PREDATOR, BLEND, SCANLINE };
    template<int N>
    void PutImageGeneric(image *dest, vec2i pos, uint8_t color,
                         image *blend, vec2i bpos,
                         uint8_t *map1, uint8_t *map2, int amount,
                         int nframes, uint8_t *tint,
                         ColorFilter *f, palette *pal);

    vec2i m_size;
    uint8_t *m_data;
};

#endif

