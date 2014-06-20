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
#include <stdlib.h>

#include "common.h"

#include "image.h"

linked_list image_list; // FIXME: only jwindow.cpp needs this

image_descriptor::image_descriptor(vec2i size,
                                   int keep_dirties, int static_memory)
{
    m_clipx1 = 0; m_clipy1 = 0;
    m_l = size.x; m_h = size.y;
    m_clipx2 = m_l; m_clipy2 = m_h;
    keep_dirt = keep_dirties;
    static_mem = static_memory;
}

void image::SetSize(vec2i new_size, uint8_t *page)
{
    DeletePage();
    m_size = new_size;
    MakePage(new_size, page);
}

void image::MakePage(vec2i size, uint8_t *page_buffer)
{
    m_data = page_buffer ? page_buffer : (uint8_t *)malloc(size.x * size.y);
}

void image::DeletePage()
{
    if(!m_special || !m_special->static_mem)
        free(m_data);
}

image::~image()
{
    if(m_locked)
    {
        fprintf(stderr, "Error: image is locked upon deletion\n");
        Unlock();
    }

    image_list.unlink(this);
    DeletePage();
    delete m_special;
}

uint8_t image::Pixel(vec2i pos)
{
    CONDITION(pos.x >= 0 && pos.x < m_size.x && pos.y >= 0 && pos.y < m_size.y,
              "image::Pixel Bad pixel xy");
    return scan_line(pos.y)[pos.x];
}

void image::PutPixel(vec2i pos, uint8_t color)
{
    CONDITION(pos.x >= 0 && pos.x < m_size.x && pos.y >= 0 && pos.y < m_size.y,
              "image::PutPixel Bad pixel xy");

    if (m_special &&
         pos.x >= m_special->x1_clip() && pos.x < m_special->x2_clip() &&
         pos.y >= m_special->y1_clip() && pos.y < m_special->y2_clip())
        return;

    scan_line(pos.y)[pos.x] = color;
}


image::image(vec2i size, uint8_t *page_buffer, int create_descriptor)
{
    m_size = size;
    m_special = NULL;
    if (create_descriptor || page_buffer)
        m_special = new image_descriptor(size, create_descriptor == 2,
                                         (page_buffer != NULL));
    MakePage(size, page_buffer);
    image_list.add_end(this);
    m_locked = false;
}

image::image(bFILE *fp, spec_entry *e /* = NULL */)
{
    if (e)
        fp->seek(e->offset, 0);
    m_size.x = fp->read_uint16();
    m_size.y = fp->read_uint16();
    m_special = NULL;
    MakePage(m_size, NULL);
    for (int i = 0; i < m_size.y; i++)
        fp->read(scan_line(i), m_size.x);
    image_list.add_end(this);
    m_locked = false;
}

void image::Lock()
{
    /* This is currently a no-op, because it's unneeded with SDL */

    if(m_locked)
        fprintf(stderr, "Trying to lock a locked picture!\n");
    m_locked = true;
}

void image::Unlock()
{
    /* This is currently a no-op, because it's unneeded with SDL */

    if(!m_locked)
        fprintf(stderr, "Trying to unlock an unlocked picture!\n");
    m_locked = false;
}

void image_uninit()
{
    while (image_list.first())
    {
        image *im = (image *)image_list.first();
        image_list.unlink(im);
        delete im;
    }
}


void image_init()
{
    ;
}

void image::clear(int16_t color)
{
    Lock();
    if(color == -1)
        color = 0; // transparent
    if(m_special)
    {
        if(m_special->x1_clip() < m_special->x2_clip())
            for(int j = m_special->y1_clip(); j <m_special->y2_clip(); j++)
                memset(scan_line(j) + m_special->x1_clip(), color,
                       m_special->x2_clip() - m_special->x1_clip());
    }
    else
        for(int j = 0; j < m_size.y; j++)
            memset(scan_line(j), color, m_size.x);
    AddDirty(0, 0, m_size.x, m_size.y);
    Unlock();
}

image *image::copy()
{
    Lock();
    image *im = new image(m_size);
    im->Lock();
    for(int j = 0; j < m_size.y; j++)
        memcpy(im->scan_line(j), scan_line(j), m_size.x);
    im->Unlock();
    Unlock();
    return im;
}

void image::line(int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint8_t color)
{
  int16_t i, xc, yc, er, n, m, xi, yi, xcxi, ycyi, xcyi;
  unsigned dcy, dcx;
  // check to make sure that both endpoint are on the screen

  int cx1, cy1, cx2, cy2;

  // check to see if the line is completly clipped off
  GetClip(cx1, cy1, cx2, cy2);
  if ((x1 < cx1 && x2 < cx1) || (x1 >= cx2 && x2 >= cx2) ||
      (y1 < cy1 && y2 < cy1) || (y1 >= cy2 && y2 >= cy2))
    return;

  if (x1>x2)        // make sure that x1 is to the left
  {
    i=x1; x1=x2; x2=i;  // if not swap points
    i=y1; y1=y2; y2=i;
  }

  // clip the left side
  if (x1<cx1)
  {
    int my=(y2-y1);
    int mx=(x2-x1), b;
    if (!mx) return ;
    if (my)
    {
      b=y1-(y2-y1)*x1/mx;
      y1=my*cx1/mx+b;
      x1=cx1;
    }
    else x1=cx1;
  }

  // clip the right side
  if (x2 >= cx2)
  {
    int my=(y2-y1);
    int mx=(x2-x1), b;
    if (!mx) return ;
    if (my)
    {
      b=y1-(y2-y1)*x1/mx;
      y2=my * (cx2 - 1) / mx + b;
      x2 = cx2 - 1;
    }
    else x2 = cx2 - 1;
  }

  if (y1>y2)        // make sure that y1 is on top
  {
    i=x1; x1=x2; x2=i;  // if not swap points
    i=y1; y1=y2; y2=i;
  }

  // clip the bottom
  if (y2 >= cy2)
  {
    int mx=(x2-x1);
    int my=(y2-y1), b;
    if (!my)
      return ;
    if (mx)
    {
      b = y1 - (y2 - y1) * x1 / mx;
      x2 = (cy2 - 1 - b) * mx / my;
      y2 = cy2 - 1;
    }
    else y2 = cy2 - 1;
  }

  // clip the top
  if (y1<cy1)
  {
    int mx=(x2-x1);
    int my=(y2-y1), b;
    if (!my) return ;
    if (mx)
    {
      b=y1-(y2-y1)*x1/mx;
      x1=(cy1-b)*mx/my;
      y1=cy1;
    }
    else y1=cy1;
  }


  // see if it got cliped into the box, out out
  if (x1<cx1 || x2<cx1 || x1 >= cx2 || x2 >= cx2 || y1<cy1 || y2 <cy1 || y1 >= cy2 || y2 >= cy2)
    return;



  if (x1>x2)
  { xc=x2; xi=x1; }
  else { xi=x2; xc=x1; }


  // assume y1<=y2 from above swap operation
  yi=y2; yc=y1;

  AddDirty(xc, yc, xi + 1, yi + 1);
  dcx=x1; dcy=y1;
  xc=(x2-x1); yc=(y2-y1);
  if (xc<0) xi=-1; else xi=1;
  if (yc<0) yi=-1; else yi=1;
  n=abs(xc); m=abs(yc);
  ycyi=abs(2*yc*xi);
  er=0;

  Lock();
  if (n>m)
  {
    xcxi=abs(2*xc*xi);
    for (i=0; i<=n; i++)
    {
      *(scan_line(dcy)+dcx)=color;
      if (er>0)
      { dcy+=yi;
    er-=xcxi;
      }
      er+=ycyi;
      dcx+=xi;
    }
  }
  else
  {
    xcyi=abs(2*xc*yi);
    for (i=0; i<=m; i++)
    {
      *(scan_line(dcy)+dcx)=color;
      if (er>0)
      { dcx+=xi;
    er-=ycyi;
      }
      er+=xcyi;
      dcy+=yi;
    }
  }
  Unlock();
}


void image::put_image(image *screen, int16_t x, int16_t y, char transparent)
{
    int16_t i, j, xl, yl;
    uint8_t *pg1, *pg2, *source, *dest;

    // the screen is clipped then we only want to put part of the image
    if(screen->m_special)
    {
        put_part(screen, x, y, 0, 0, m_size.x-1, m_size.y-1, transparent);
        return;
    }

    if(x < screen->Size().x && y < screen->Size().y)
    {
        xl = m_size.x;
        if(x + xl > screen->Size().x) // clip to the border of the screen
            xl = screen->Size().x - x;
        yl = m_size.y;
        if(y + yl > screen->Size().y)
            yl = screen->Size().y - y;

        int startx = 0, starty = 0;
        if(x < 0)
        {
            startx = -x;
            x = 0;
        }
        if(y < 0)
        {
            starty = -y;
            y = 0;
        }

        if(xl < 0 || yl < 0)
            return;

        screen->AddDirty(x, y, x + xl, y + yl);
        screen->Lock();
        Lock();
        for(j = starty; j < yl; j++, y++)
        {
            pg1 = screen->scan_line(y);
            pg2 = scan_line(j);
            if(transparent)
            {
                for(i = startx, source = pg2+startx, dest = pg1 + x;
                    i < xl;
                    i++, source++, dest++)
                {
                    if (*source)
                        *dest = *source;
                }
            }
            else
                memcpy(&pg1[x], pg2, xl); // straight copy
        }
        Unlock();
        screen->Unlock();
    }
}

void image::fill_image(image *screen, int16_t x1, int16_t y1, int16_t x2, int16_t y2, int16_t align)
{
  int16_t i, j, w, xx, start, xl, starty;
  uint8_t *pg1, *pg2;
  CHECK(x1<=x2 && y1<=y2);  // we should have gotten this

  if (screen->m_special)
  { x1=screen->m_special->bound_x1(x1);
    y1=screen->m_special->bound_y1(y1);
    x2=screen->m_special->bound_x2(x2+1)-1;
    y2=screen->m_special->bound_y2(y2+1)-1;
  }
  else
  { if (x1<0) x1=0;
    if (y2<0) y1=0;
    if (x2>=screen->Size().x)  x2=screen->Size().x-1;
    if (y2>=screen->Size().y) y2=screen->Size().y-1;
  }
  if (x2<0 || y2<0 || x1>=screen->Size().x || y1>=screen->Size().y)
    return ;
  screen->AddDirty(x1, y1, x2 + 1, y2 + 1);
  w=m_size.x;
  if (align)
  {
    start=x1%w;
    starty=y1%m_size.y;
  }
  else
  { start=0;
    starty=0;
  }
  screen->Lock();
  Lock();
  for (j=y1; j<=y2; j++)
  {
    pg1=screen->scan_line(j);
    pg2=scan_line(starty++);
    if (starty>=m_size.y) starty=0;
    i=x1;
    xx=start;
    while (i<=x2)
    {
      xl=Min(w-xx, x2-i+1);

      memcpy(&pg1[i], &pg2[xx], xl);
      xx=0;
      i+=xl;
    }
  }
  Unlock();
  screen->Unlock();
}


void image::put_part(image *screen, int16_t x, int16_t y,
        int16_t x1, int16_t y1, int16_t x2, int16_t y2, char transparent)
{
  int16_t xlen, ylen, j, i;
  uint8_t *pg1, *pg2, *source, *dest;
  CHECK(x1<=x2 && y1<=y2);

  int cx1, cy1, cx2, cy2;
  screen->GetClip(cx1, cy1, cx2, cy2);


  // see if the are to be put is outside of actual image, if so adjust
  // to fit in the image
  if (x1<0) { x+=-x1; x1=0; }
  if (y1<0) { y+=-y1; y1=0; }
  if (x2>=m_size.x) x2=m_size.x-1;
  if (y2>=m_size.y) y2=m_size.y-1;
  if (x1>x2 || y1>y2) return ;      // return if it was adjusted so that nothing will be put


  // see if the image gets clipped off the screen
  if (x >= cx2 || y >= cy2 || x + (x2 - x1) < cx1 || y + (y2 - y1) < cy1)
    return ;


  if (x<cx1)
  { x1+=(cx1-x); x=cx1; }
  if (y<cy1)
  { y1+=(cy1-y); y=cy1; }

  if (x + x2 - x1 + 1 >= cx2)
  { x2 = cx2 - 1 - x + x1; }

  if (y + y2 - y1 + 1 >= cy2)
  { y2 = cy2 - 1 - y + y1; }
  if (x1>x2 || y1>y2) return ;




  xlen=x2-x1+1;
  ylen=y2-y1+1;

  screen->AddDirty(x, y, x + xlen, y + ylen);

  screen->Lock();
  Lock();
  pg1=screen->scan_line(y);
  pg2=scan_line(y1);

  if (transparent)
  {
    for (j=0; j<ylen; j++)
    {
      for (i=0, source=&pg2[x1], dest=&pg1[x]; i<xlen; i++, source++, dest++)
        if (*source) *dest=*source;
      pg1=screen->next_line(y+j, pg1);
      pg2=next_line(y1+j, pg2);
    }
  }
  else
  for (j=0; j<ylen; j++)
  {
    memcpy(&pg1[x], &pg2[x1], xlen);   // strait copy
    pg1=screen->next_line(y+j, pg1);
    pg2=next_line(y1+j, pg2);
  }
  Unlock();
  screen->Unlock();
}

void image::put_part_xrev(image *screen, int16_t x, int16_t y,
        int16_t x1, int16_t y1, int16_t x2, int16_t y2, char transparent)
{
  int16_t xl, yl, j, i;
  uint8_t *pg1, *pg2, *source, *dest;
  CHECK(x1<=x2 && y1<=y2);

  i=x1; x1=m_size.x-x2-1;  // reverse the x locations
  x2=m_size.x-i-1;

  if (x1<0)
  { x-=x1; x1=0; }
  if (y1<0)
  { y-=y1; y1=0; }

  if (screen->m_special)
  {
    int cx1, cy1, cx2, cy2;
    screen->m_special->GetClip(cx1, cy1, cx2, cy2);
    // FIXME: don't we need < cx1 instead of < 0 here?
    if (x >= cx2 || y >= cy2 || x + (x2 - x1) < 0 || y + (y2 - y1) < 0)
      return;
    if (x<cx1)
    { x1+=(cx1-x); x=cx1; }
    if (y<cy1)
    { y1+=(cy1-y); y=cy1; }
    if (x + x2 - x1 + 1 >= cx2)
    { x2 = cx2 - 1 - x + x1; }
    if (y + y2 - y1 + 1 >= cy2)
    { y2 = cy2 - 1 - y + y1; }
  }
  else  if (x>screen->Size().x || y>screen->Size().y || x+x2<0 || y+y2<0)
    return ;

  if (x<screen->Size().x && y<screen->Size().y && x1<m_size.x && y1<m_size.y &&
      x1<=x2 && y1<=y2)
  {
    if (x2>=m_size.x)
      x2=m_size.x-1;
    if (y2>=m_size.y)
      y2=m_size.y-1;
    xl=x2-x1+1;
    if (x+xl>screen->Size().x)
      xl=screen->Size().x-x;
    yl=y2-y1+1;
    if (y+yl>screen->Size().y)
      yl=screen->Size().y-y;
    screen->AddDirty(x, y, x + xl, y + yl);
    screen->Lock();
    Lock();
    for (j=0; j<yl; j++)
    {
      pg1=screen->scan_line(y+j);
      pg2=scan_line(y1+j);
      if (transparent)
      {
    for (i=0, source=&pg2[x1], dest=&pg1[x+xl-1]; i<xl; i++, source++, dest--)
          if (*source) *dest=*source;
      }
      else
    for (i=0, source=&pg2[x1], dest=&pg1[x+xl-1]; i<xl; i++, source++, dest++)
          *dest=*source;
    }
    Unlock();
    screen->Unlock();
  }
}


void image::put_part_masked(image *screen, image *mask, int16_t x, int16_t y,
        int16_t maskx, int16_t masky,
        int16_t x1, int16_t y1, int16_t x2, int16_t y2)
{
  int16_t xl, yl, j, i, ml, mh;
  uint8_t *pg1, *pg2, *pg3;
  CHECK(x1<=x2 && y1<=y2);

  if (screen->m_special)
  {
    int cx1, cy1, cx2, cy2;
    screen->m_special->GetClip(cx1, cy1, cx2, cy2);
    if (x >= cx2 || y >= cy2 || x+(x2-x1)<0 || y+(y2-y1)<0) return ;
    if (x<cx1)
    { x1+=(cx1-x); x=cx1; }
    if (y<cy1)
    { y1+=(cy1-y); y=cy1; }
    if (x + x2 - x1 >= cx2)
    { x2 = cx2 - 1 + x1 - x; }
    if (y + y2 - y1 >= cy2)
    { y2 = cy2 - 1 + y1 - y; }
  }
  else  if (x>screen->Size().x || y>screen->Size().y || x+x1<0 || y+y1<0)
    return ;

  ml=mask->Size().x;
  mh=mask->Size().y;
  if (x<screen->Size().x && y<screen->Size().y && x1<m_size.x && y1<m_size.y &&
      maskx<ml && masky<mh && x1<=x2 && y1<=y2)
  {

    if (x2>=m_size.x)
      x2=m_size.x-1;
    if (y2>=m_size.y)
      y2=m_size.y-1;
    xl=x2-x1+1;
    if (x+xl>screen->Size().x)
      xl=screen->Size().x-x-1;
    yl=y2-y1+1;
    if (y+yl>screen->Size().y)
      yl=screen->Size().y-y-1;
    screen->AddDirty(x, y, x + xl, y + yl);
    screen->Lock();
    mask->Lock();
    Lock();
    for (j=0; j<yl; j++)
    {
      pg1=screen->scan_line(y+j);
      pg2=scan_line(y1+j);
      pg3=mask->scan_line(masky++);
      if (masky>=mh)           // wrap the mask around if out of bounds
    masky=0;
      for (i=0; i<xl; i++)
      {
    if (pg3[maskx+i])          // check to make sure not 0 before putting
      pg1[x+i]=pg2[x1+i];
    if (maskx>=ml)            // wrap x around if it goes to far
      maskx=0;
      }
    }
    Unlock();
    mask->Unlock();
    screen->Unlock();
  }
}

void image::rectangle(int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint8_t color)
{
  line(x1, y1, x2, y1, color);
  line(x2, y1, x2, y2, color);
  line(x1, y2, x2, y2, color);
  line(x1, y1, x1, y2, color);
}

void image::SetClip(int x1, int y1, int x2, int y2)
{
    // If the image does not already have an Image descriptor, allocate one
    // with no dirty rectangle keeping.
    if(!m_special)
        m_special = new image_descriptor(m_size.x, m_size.y, 0);

    // set the image descriptor what the clip
    // should be it will adjust to fit within the image.
    m_special->SetClip(x1, y1, x2, y2);
}

void image::GetClip(int &x1, int &y1, int &x2, int &y2)
{
    if (m_special)
        m_special->GetClip(x1, y1, x2, y2);
    else
    {
        x1 = 0; y1 = 0; x2 = m_size.x; y2 = m_size.y;
    }
}

void image::InClip(int x1, int y1, int x2, int y2)
{
    if (m_special)
    {
        x1 = Min(x1, m_special->x1_clip());
        y1 = Min(y1, m_special->y1_clip());
        x2 = Max(x2, m_special->x2_clip());
        y2 = Max(y2, m_special->y2_clip());
    }

    SetClip(x1, y1, x2, y2);
}

//
// reduce the number of dirty rectanges to 1 by finding the minmum area that
// can contain all the rectangles and making this the new dirty area
//
void image_descriptor::ReduceDirties()
{
    dirty_rect *p = (dirty_rect *)dirties.first();
    int x1 = 6000, y1 = 6000, x2 = -1, y2 = -1;

    for (int i = dirties.Count(); i--; )
    {
        x1 = Min(x1, p->dx1); y1 = Min(y1, p->dy1);
        x2 = Max(x1, p->dx1); y2 = Max(y1, p->dy1);
        dirty_rect *tmp = (dirty_rect *)p->Next();
        dirties.unlink(p);
        delete p;
        p = tmp;
    }
    dirties.add_front(new dirty_rect(x1, y1, x2, y2));
}

void image_descriptor::delete_dirty(int x1, int y1, int x2, int y2)
{
    int ax1, ay1, ax2, ay2;
    dirty_rect *p, *next;

    if (!keep_dirt)
        return;

    x1 = Max(0, x1); x2 = Min(m_l, x2);
    y1 = Max(0, y1); y2 = Min(m_h, y2);

    if (x1 >= x2 || y1 >= y2)
        return;

    int i = dirties.Count();
    if (!i)
        return;

    for (p = (dirty_rect *)dirties.first(); i; i--, p = next)
    {
        next = (dirty_rect *)p->Next();

        // are the two touching?
        if (x2 <= p->dx1 || y2 <= p->dy1 || x1 > p->dx2 || y1 > p->dy2)
            continue;

        // does it take a x slice off? (across)
        if (x2 >= p->dx2 + 1 && x1 <= p->dx1)
        {
            if (y2 >= p->dy2 + 1 && y1 <= p->dy1)
            {
                dirties.unlink(p);
                delete p;
            }
            else if (y2 >= p->dy2 + 1)
                p->dy2 = y1 - 1;
            else if (y1 <= p->dy1)
                p->dy1 = y2;
            else
            {
                dirties.add_front(new dirty_rect(p->dx1, p->dy1, p->dx2, y1-1));
                p->dy1 = y2;
            }
        }
        // does it take a y slice off (down)
        else if (y2 - 1>=p->dy2 && y1<=p->dy1)
        {
            if (x2 - 1>=p->dx2)
                p->dx2=x1-1;
            else if (x1<=p->dx1)
                p->dx1=x2;
            else
            {
                dirties.add_front(new dirty_rect(p->dx1, p->dy1, x1-1, p->dy2));
                p->dx1=x2;
            }
        }
        // otherwise it just takes a little chunk off
        else
        {
            if (x2 - 1>=p->dx2)      { ax1=p->dx1; ax2=x1; }
            else if (x1<=p->dx1) { ax1=x2; ax2=p->dx2+1; }
            else                { ax1=p->dx1; ax2=x1; }
            if (y2 - 1>=p->dy2)      { ay1=y1; ay2=p->dy2+1; }
            else if (y1<=p->dy1) { ay1=p->dy1; ay2=y2; }
            else                { ay1=y1; ay2=y2; }
            dirties.add_front(new dirty_rect(ax1, ay1, ax2-1, ay2-1));

            if (x2 - 1>=p->dx2 || x1<=p->dx1)  { ax1=p->dx1; ax2=p->dx2+1; }
            else                         { ax1=x2; ax2=p->dx2+1; }

            if (y2 - 1>=p->dy2)
            { if (ax1==p->dx1) { ay1=p->dy1; ay2=y1; }
                          else { ay1=y1; ay2=p->dy2+1;   } }
            else if (y1<=p->dy1) { if (ax1==p->dx1) { ay1=y2; ay2=p->dy2+1; }
                                             else  { ay1=p->dy1; ay2=y2; } }
            else           { if (ax1==p->dx1) { ay1=p->dy1; ay2=y1; }
                             else { ay1=y1; ay2=y2; } }
            dirties.add_front(new dirty_rect(ax1, ay1, ax2 - 1, ay2 - 1));

            if (x1>p->dx1 && x2 - 1<p->dx2)
            {
                if (y1>p->dy1 && y2 - 1<p->dy2)
                {
                    dirties.add_front(new dirty_rect(p->dx1, p->dy1, p->dx2, y1-1));
                    dirties.add_front(new dirty_rect(p->dx1, y2, p->dx2, p->dy2));
                }
                else if (y1<=p->dy1)
                    dirties.add_front(new dirty_rect(p->dx1, y2, p->dx2, p->dy2));
                else
                    dirties.add_front(new dirty_rect(p->dx1, p->dy1, p->dx2, y1-1));
            }
            else if (y1>p->dy1 && y2 - 1<p->dy2)
                dirties.add_front(new dirty_rect(p->dx1, y2, p->dx2, p->dy2));
            dirties.unlink(p);
            delete p;
        }
    }
}

// specifies that an area is a dirty
void image_descriptor::AddDirty(int x1, int y1, int x2, int y2)
{
    dirty_rect *p;
    if (!keep_dirt)
        return;

    x1 = Max(0, x1); x2 = Min(m_l, x2);
    y1 = Max(0, y1); y2 = Min(m_h, y2);

    if (x1 >= x2 || y1 >= y2)
        return;

    int i = dirties.Count();
    if (!i)
        dirties.add_front(new dirty_rect(x1, y1, x2 - 1, y2 - 1));
    else if (i >= MAX_DIRTY)
    {
        dirties.add_front(new dirty_rect(x1, y1, x2 - 1, y2 - 1));
        ReduceDirties();  // reduce to one dirty rectangle, we have to many
    }
    else
    {
      for (p=(dirty_rect *)dirties.first(); i>0; i--)
      {

        // check to see if this new rectangle completly encloses the check rectangle
        if (x1<=p->dx1 && y1<=p->dy1 && x2>=p->dx2+1 && y2>=p->dy2+1)
        {
          dirty_rect *tmp=(dirty_rect*) p->Next();
          dirties.unlink(p);
          delete p;
          if (!dirties.first())
              i=0;
          else p=tmp;
        }
        else if (!(x2 - 1 <p->dx1 || y2 - 1 <p->dy1 || x1>p->dx2 || y1>p->dy2))
        {



/*          if (x1<=p->dx1) { a+=p->dx1-x1; ax1=x1; } else ax1=p->dx1;
          if (y1<=p->dy1) { a+=p->dy1-y1; ay1=y1; } else ay1=p->dy1;
          if (x2 - 1 >=p->dx2) { a+=x2 - 1 -p->dx2; ax2=x2 - 1; } else ax2=p->dx2;
          if (y2 - 1 >=p->dy2) { a+=y2 - 1 -p->dy2; ay2=y2 - 1; } else ay2=p->dy2;

      if (a<50)
      { p->dx1=ax1;                         // then expand the dirty
        p->dy1=ay1;
        p->dx2=ax2;
        p->dy2=ay2;
        return ;
      }
      else */
            {
              if (x1<p->dx1)
                AddDirty(x1, Max(y1, p->dy1), p->dx1, Min(y2, p->dy2 + 1));
              if (x2>p->dx2+1)
                AddDirty(p->dx2+1, Max(y1, p->dy1), x2, Min(y2, p->dy2 + 1));
              if (y1<p->dy1)
                AddDirty(x1, y1, x2, p->dy1);
              if (y2 - 1>p->dy2)
                AddDirty(x1, p->dy2+1, x2, y2);
              return ;
            }
            p=(dirty_rect *)p->Next();
          } else p=(dirty_rect *)p->Next();

      }
      CHECK(x1 < x2 && y1 < y2);
      dirties.add_end(new dirty_rect(x1, y1, x2 - 1, y2 - 1));
    }
}

void image::bar      (int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint8_t color)
{
  int16_t y;
  if (x1>x2 || y1>y2) return ;
  if (m_special)
  { x1=m_special->bound_x1(x1);
    y1=m_special->bound_y1(y1);
    x2=m_special->bound_x2(x2+1)-1;
    y2=m_special->bound_y2(y2+1)-1;
  }
  else
  { if (x1<0) x1=0;
    if (y1<0) y1=0;
    if (x2>=m_size.x)  x2=m_size.x-1;
    if (y2>=m_size.y) y2=m_size.y-1;
  }
  if (x2<0 || y2<0 || x1>=m_size.x || y1>=m_size.y || x2<x1 || y2<y1)
    return ;
  Lock();
  for (y=y1; y<=y2; y++)
    memset(scan_line(y)+x1, color, (x2-x1+1));
  Unlock();
  AddDirty(x1, y1, x2 + 1, y2 + 1);
}

void image::xor_bar  (int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint8_t color)
{
  int16_t y, x;
  if (x1>x2 || y1>y2) return ;
  if (m_special)
  { x1=m_special->bound_x1(x1);
    y1=m_special->bound_y1(y1);
    x2=m_special->bound_x2(x2+1)-1;
    y2=m_special->bound_y2(y2+1)-1;
  }
  else
  { if (x1<0) x1=0;
    if (y1<0) y1=0;
    if (x2>=m_size.x)  x2=m_size.x-1;
    if (y2>=m_size.y) y2=m_size.y-1;
  }
  if (x2<0 || y2<0 || x1>=m_size.x || y1>=m_size.y || x2<x1 || y2<y1)
    return ;

  Lock();
  uint8_t *sl=scan_line(y1)+x1;
  for (y=y1; y<=y2; y++)
  {
    uint8_t *s=sl;
    for (x=x1; x<=x2; x++, s++)
      *s=(*s)^color;
    sl+=m_size.x;
  }
  Unlock();

  AddDirty(x1, y1, x2 + 1, y2 + 1);
}


void image::unpack_scanline(int16_t line, char bitsperpixel)
{
  int16_t x;
  uint8_t *sl, *ex, mask, bt, sh;
  ex=(uint8_t *)malloc(m_size.x);

  Lock();
  sl=scan_line(line);
  memcpy(ex, sl, m_size.x);
  Unlock();

  if (bitsperpixel==1)      { mask=128;           bt=8; }
  else if (bitsperpixel==2) { mask=128+64;        bt=4; }
  else                 {  mask=128+64+32+16; bt=2; }

  for (x=0; x<m_size.x; x++)
  { sh=((x%bt)<<(bitsperpixel-1));
    sl[x]=(ex[x/bt]&(mask>>sh))>>(bt-sh-1);
  }

  free((char *)ex);
}

void image::dither(palette *pal)
{
  int16_t x, y, i, j;
  uint8_t dt_matrix[]={ 0,  136, 24, 170,
           68, 204, 102, 238,
           51, 187, 17, 153,
           119, 255, 85, 221};

  uint8_t *sl;
  Lock();
  for (y = 0; y < m_size.y; y++)
  {
    sl=scan_line(y);
    for (i=0, j=y%4, x=0; x < m_size.x; x++)
      sl[x] = (pal->red(sl[x]) > dt_matrix[j * 4 + (x & 3)]) ? 255 : 0;
  }
  Unlock();
}

void image_descriptor::ClearDirties()
{
    dirty_rect *dr = (dirty_rect *)dirties.first();
    while (dr)
    {
        dirties.unlink(dr);
        delete dr;
        dr = (dirty_rect *)dirties.first();
    }
}

void image::Scale(vec2i new_size)
{
    vec2i old_size = m_size;
    uint8_t *im = (uint8_t *)malloc(old_size.x * old_size.y);
    Lock();
    memcpy(im, scan_line(0), old_size.x * old_size.y);

    DeletePage();
    MakePage(new_size, NULL);
    m_size = new_size; // set the new height and width

    uint8_t *sl1, *sl2;
    int y, y2, x2;
    double yc, xc, yd, xd;

    yc = (double)old_size.y / (double)new_size.y;
    xc = (double)old_size.x / (double)new_size.x;
    for (y2 = 0, yd = 0; y2 < new_size.y; yd += yc, y2++)
    {
        y = (int)yd;
        sl1 = im + y * old_size.x;
        sl2 = scan_line(y2);
        for (xd = 0, x2 = 0; x2 < new_size.x; xd += xc, x2++)
            sl2[x2] = sl1[(int)xd];
    }
    free(im);
    if (m_special)
        m_special->Resize(new_size);
    Unlock();
}

void image::scroll(int16_t x1, int16_t y1, int16_t x2, int16_t y2, int16_t xd, int16_t yd)
{
  CHECK(x1>=0 && y1>=0 && x1<x2 && y1<y2 && x2<m_size.x && y2<m_size.y);
  if (m_special)
  {
    int cx1, cy1, cx2, cy2;
    m_special->GetClip(cx1, cy1, cx2, cy2);
    x1=Max(x1, cx1); y1=Max(cy1, y1); x2=Min(x2, cx2 - 1); y2=Min(y2, cy2 - 1);
  }
  int16_t xsrc, ysrc, xdst, ydst, xtot=x2-x1-abs(xd)+1, ytot, xt;
  uint8_t *src, *dst;
  if (xd<0) { xsrc=x1-xd; xdst=x1; } else { xsrc=x2-xd; xdst=x2; }
  if (yd<0) { ysrc=y1-yd; ydst=y1; } else { ysrc=y2-yd; ydst=y2; }
  for (ytot=y2-y1-abs(yd)+1; ytot; ytot--)
  { src=scan_line(ysrc)+xsrc;
    dst=scan_line(ydst)+xdst;
    if (xd<0)
      for (xt = 0; xt < xtot; xt++)
        *dst++ = *src++;
      else for (xt = 0; xt < xtot; xt++)
        *dst-- = *src--;
    if (yd<0) { ysrc++; ydst++; } else { ysrc--; ydst--; }
  }
  AddDirty(x1, y1, x2 + 1, y2 + 1);
}


image *image::create_smooth(int16_t smoothness)
{
  int16_t i, j, k, l, t, d;
  image *im;
  CHECK(smoothness>=0);
  if (!smoothness) return NULL;
  d=smoothness*2+1;
  d=d*d;
  im=new image(m_size);
  for (i=0; i<m_size.x; i++)
    for (j=0; j<m_size.y; j++)
    {
      for (t=0, k=-smoothness; k<=smoothness; k++)
    for (l=-smoothness; l<=smoothness; l++)
      if (i+k>smoothness && i+k<m_size.x-smoothness && j+l<m_size.y-smoothness && j+l>smoothness)
        t+=Pixel(vec2i(i+k, j+l));
      else t+=Pixel(vec2i(i, j));
      im->PutPixel(vec2i(i, j), t/d);
    }
  return im;
}

void image::widget_bar(int16_t x1, int16_t y1, int16_t x2, int16_t y2,
       uint8_t light, uint8_t med, uint8_t dark)
{
  line(x1, y1, x2, y1, light);
  line(x1, y1, x1, y2, light);
  line(x2, y1+1, x2, y2, dark);
  line(x1+1, y2, x2-1, y2, dark);
  bar(x1+1, y1+1, x2-1, y2-1, med);
}

class fill_rec
{
public :
  int16_t x, y;
  fill_rec *last;
  fill_rec(int16_t X, int16_t Y, fill_rec *Last)
  { x=X; y=Y; last=Last; }
} ;

void image::flood_fill(int16_t x, int16_t y, uint8_t color)
{
  uint8_t *sl, *above, *below;
  fill_rec *recs=NULL, *r;
  uint8_t fcolor;
  Lock();
  sl=scan_line(y);
  fcolor=sl[x];
  if (fcolor==color) return ;
  do
  {
    if (recs)
    { r=recs;
      recs=recs->last;
      x=r->x; y=r->y;
      delete r;
    }
    sl=scan_line(y);
    if (sl[x]==fcolor)
    {
      while (sl[x]==fcolor && x>0) x--;
      if (sl[x]!=fcolor) x++;
      if (y>0)
      {
        above=scan_line(y-1);
        if (above[x]==fcolor)
        { r=new fill_rec(x, y-1, recs);
          recs=r;
        }
      }
      if (y<m_size.y-1)
      {
        above=scan_line(y+1);
        if (above[x]==fcolor)
        { r=new fill_rec(x, y+1, recs);
          recs=r;
        }
      }



      do
      {
        sl[x]=color;
        if (y>0)
        { above=scan_line(y-1);
          if (x>0 && above[x-1]!=fcolor && above[x]==fcolor)
          { r=new fill_rec(x, y-1, recs);
            recs=r;
          }
        }
        if (y<m_size.y-1)
        { below=scan_line(y+1);
          if (x>0 && below[x-1]!=fcolor && below[x]==fcolor)
          { r=new fill_rec(x, y+1, recs);
            recs=r;
          }
        }
        x++;
      } while (sl[x]==fcolor && x<m_size.x);
      x--;
      if (y>0)
      {
        above=scan_line(y-1);
        if (above[x]==fcolor)
        { r=new fill_rec(x, y-1, recs);
          recs=r;
        }
      }
      if (y<m_size.y-1)
      {
        above=scan_line(y+1);
        if (above[x]==fcolor)
        { r=new fill_rec(x, y+1, recs);
          recs=r;
        }
      }
    }
  } while (recs);
  Unlock();
}


#define LED_L 5
#define LED_H 5
void image::burn_led(int16_t x, int16_t y, int32_t num, int16_t color, int16_t scale)
{
  char st[100];
  int16_t ledx[]={ 1, 2, 1, 2, 3, 3, 3, 3, 1, 2, 0, 0, 0, 0};
  int16_t ledy[]={ 3, 3, 0, 0, 1, 2, 4, 6, 7, 7, 4, 6, 1, 2};

  int16_t dig[]={ 2+4+8+16+32+64, 4+8, 2+4+1+32+16, 2+4+1+8+16, 64+1+4+8,
             2+64+1+8+16, 64+32+1+8+16, 2+4+8, 1+2+4+8+16+32+64, 64+2+4+1+8, 1};
  int16_t xx, yy, zz;
  sprintf(st, "%8ld", (long int)num);
  for (xx=0; xx<8; xx++)
  {
    if (st[xx]!=' ')
    {
      if (st[xx]=='-')
    zz=10;
      else
    zz=st[xx]-'0';
      for (yy=0; yy<7; yy++)
    if ((1<<yy)&dig[zz])
      line(x+ledx[yy*2]*scale, y+ledy[yy*2]*scale, x+ledx[yy*2+1]*scale,
        y+ledy[yy*2+1]*scale, color);
    }
    x+=6*scale;
  }
}

uint8_t dither_matrix[]={ 0,  136, 24, 170,
             68, 204, 102, 238,
             51, 187, 17, 153,
             119, 255, 85, 221};

image *image::copy_part_dithered (int16_t x1, int16_t y1, int16_t x2, int16_t y2)
{
  int x, y, cx1, cy1, cx2, cy2, ry, rx, bo, dity, ditx;
  image *ret;
  uint8_t *sl1, *sl2;
  GetClip(cx1, cy1, cx2, cy2);
  if (y1<cy1) y1=cy1;
  if (x1<cx1) x1=cx1;
  if (y2>cy2 - 1) y2=cy2 - 1;
  if (x2>cx2 - 1) x2=cx2 - 1;
  CHECK(x2>=x1 && y2>=y1);
  if (x2<x1 || y2<y1) return NULL;
  ret=new image(vec2i((x2-x1+8)/8, (y2-y1+1)));
  if (!last_loaded())
    ret->clear();
  else
  {
    ret->Lock();
    Lock();
    for (y=y1, ry=0, dity=(y1%4)*4; y<=y2; y++, ry++)
    {
      sl1=ret->scan_line(ry);     // sl1 is the scan linefo the return image
      sl2=scan_line(y);          // sl2 is the orginal image scan line
      memset(sl1, 0, (x2-x1+8)/8);
      for (bo=7, rx=0, x=x1, ditx=x1%4; x<=x2; x++)
      {
        if (last_loaded()->red(sl2[x])>dither_matrix[ditx+dity])
          sl1[rx]|=1<<bo;
        if (bo!=0)
      bo--;
        else
        {
        rx++;
      bo=7;
        }
        ditx+=1; if (ditx>3) ditx=0;
      }
      dity+=4; if (dity>12) dity=0;
    }
    Unlock();
    ret->Unlock();
  }
  return ret;
}

void image::FlipX()
{
    Lock();
    for (int y = 0; y < m_size.y; y++)
    {
        uint8_t *sl = scan_line(y);
        for (int x = 0; x < m_size.x / 2; x++)
        {
            uint8_t tmp = sl[x];
            sl[x] = sl[m_size.x - 1 - x];
            sl[m_size.x - 1 - x] = tmp;
        }
    }
    Unlock();
}

void image::FlipY()
{
    Lock();
    for (int y = 0; y < m_size.y / 2; y++)
    {
        uint8_t *sl1 = scan_line(y);
        uint8_t *sl2 = scan_line(m_size.y - 1 - y);
        for (int x = 0; x < m_size.x; x++)
        {
            uint8_t tmp = sl1[x];
            sl1[x] = sl2[x];
            sl2[x] = tmp;
        }
    }
    Unlock();
}

