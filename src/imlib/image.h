/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef _IMAGE_HPP_
#define _IMAGE_HPP_

#include <stdlib.h>
#include "linked.h"
#include "palette.h"
#include "specs.h"
#define MAX_DIRTY 200

void image_init();
void image_uninit();
extern linked_list image_list;

class dirty_rect : public linked_node
{
public :
  int16_t dx1,dy1,dx2,dy2;
  dirty_rect(int16_t x1, int16_t y1, int16_t x2, int16_t y2)
  { dx1=x1; dy1=y1; dx2=x2; dy2=y2;
    if(x2<x1 || y2<y1)
      printf("add incorrect dirty\n");
  }
  virtual int16_t compare(void *n1, int16_t field)
  { return((dirty_rect *)n1)->dy1>dy1; }
} ;

class image_descriptor
{
private:
    int m_l, m_h;
    int m_clipx1, m_clipy1, m_clipx2, m_clipy2;

public:
    uint8_t keep_dirt,
            static_mem; // if set, don't free memory on exit

    linked_list dirties;
    void *extended_descriptor;

    image_descriptor(vec2i size, int keep_dirties = 1, int static_memory = 0);
    int bound_x1(int x1) { return x1 < m_clipx1 ? m_clipx1 : x1; }
    int bound_y1(int y1) { return y1 < m_clipy1 ? m_clipy1 : y1; }
    int bound_x2(int x2) { return x2 > m_clipx2 ? m_clipx2 : x2; }
    int bound_y2(int y2) { return y2 > m_clipy2 ? m_clipy2 : y2; }
    inline int x1_clip() { return m_clipx1; }
    inline int y1_clip() { return m_clipy1; }
    inline int x2_clip() { return m_clipx2; }
    inline int y2_clip() { return m_clipy2; }
    void ClearDirties();
    void GetClip(int &x1, int &y1, int &x2, int &y2)
    {
        x1 = m_clipx1; y1 = m_clipy1; x2 = m_clipx2; y2 = m_clipy2;
    }
    void SetClip(int x1, int y1, int x2, int y2)
    {
        if(x2 < x1 + 1) x2 = x1 + 1;
        if(y2 < y1 + 1) y2 = y1 + 1;
        m_clipx1 = Max(x1, 0); m_clipy1 = Max(y1, 0);
        m_clipx2 = Min(x2, m_l); m_clipy2 = Min(y2, m_h);
    }
    void ReduceDirties();
    void AddDirty(int x1, int y1, int x2, int y2);
    void delete_dirty(int x1, int y1, int x2, int y2);
    void Resize(vec2i size)
    {
        m_l = size.x; m_h = size.y;
        m_clipx1 = 0; m_clipy1 = 0; m_clipx2 = m_l; m_clipy2 = m_h;
    }
};

class image : public linked_node
{
private:
    uint8_t *m_data;
    vec2i m_size;
    bool m_locked;

    void MakePage(vec2i size, uint8_t *page_buffer);
    void DeletePage();

public:
    image_descriptor *m_special;

    image(bFILE *fp, spec_entry *e = NULL);
    image(vec2i size, uint8_t *page_buffer = NULL, int create_descriptor = 0);
    ~image();

    void Lock();
    void Unlock();

    uint8_t Pixel(vec2i pos);
    void PutPixel(vec2i pos, uint8_t color);

    inline uint8_t *scan_line(int16_t y)
    {
        return m_data + y * m_size.x;
    }
    inline uint8_t *next_line(int16_t lasty, uint8_t *last_scan)
    {
        return last_scan + m_size.x;
    }
    image *copy(); // makes a copy of an image
    void clear(int16_t color = -1); // -1 is background color

    vec2i Size() const { return m_size; }

    void scroll(int16_t x1, int16_t y1, int16_t x2, int16_t y2,
                int16_t xd, int16_t yd);
    void fill_image(image *screen, int16_t x1, int16_t y1,
                    int16_t x2, int16_t y2, int16_t align = 1);
    void put_image(image *screen, int16_t x, int16_t y, char transparent = 0);
    void put_part(image *screen, int16_t x, int16_t y, int16_t x1, int16_t y1,
                  int16_t x2, int16_t y2, char transparent = 0);
    void put_part_xrev(image *screen, int16_t x, int16_t y,
                       int16_t x1, int16_t y1, int16_t x2, int16_t y2,
                       char transparent = 0);
    void put_part_masked(image *screen, image *mask, int16_t x, int16_t y,
                         int16_t maskx, int16_t masky, int16_t x1, int16_t y1,
                         int16_t x2, int16_t y2);
    image *copy_part_dithered(int16_t x1, int16_t y1, int16_t x2, int16_t y2);
    void bar(int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint8_t color);
    void xor_bar(int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint8_t color);
    void widget_bar(int16_t x1, int16_t y1, int16_t x2, int16_t y2,
                    uint8_t light, uint8_t med, uint8_t dark);
    void line(int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint8_t color);
    void rectangle(int16_t x1, int16_t y1, int16_t x2, int16_t y2,
                   uint8_t color);
    void burn_led(int16_t x, int16_t y, int32_t num, int16_t color,
                  int16_t scale = 1);
    void SetClip(int x1, int y1, int x2, int y2);
    void GetClip(int &x1, int &y1, int &x2, int &y2);
    void InClip(int x1, int y1, int x2, int y2);

    void dirt_off()
    {
        if(m_special && m_special->keep_dirt) m_special->keep_dirt = 0;
    }
    void dirt_on()
    {
        if(m_special) m_special->keep_dirt = 1;
    }

    void AddDirty(int x1, int y1, int x2, int y2)
    {
        if (m_special) m_special->AddDirty(x1, y1, x2, y2);
    }
    void delete_dirty(int x1, int y1, int x2, int y2)
    {
        if(m_special) m_special->delete_dirty(x1, y1, x2, y2);
    }
    void ClearDirties()
    {
        if (m_special) m_special->ClearDirties();
    }
    void dither(palette *pal); // use a b&w palette!
    void Scale(vec2i size);
    void SetSize(vec2i size, uint8_t *page = NULL);
    void flood_fill(int16_t x, int16_t y, uint8_t color);
    image *create_smooth(int16_t smoothness = 1); // 0 no smoothness
    void unpack_scanline(int16_t line, char bitsperpixel = 1);
    void FlipX();
    void FlipY();
};

class image_controller
{
public:
    image_controller()
    {
        image_init();
    }
    ~image_controller()
    {
        image_uninit();
    }
};

#endif /* _IMAGE_HPP_ */

