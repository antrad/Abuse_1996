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

#include "transp.h"

void transp_put(image *im, image *screen, uint8_t *table, int x, int y)
{
    ivec2 caa, cbb;
    screen->GetClip(caa, cbb);

    ivec2 aa(0), bb = im->Size();
    ivec2 pos(x, y);

    aa += Max(caa - pos, ivec2(0));
    bb -= Max(caa - pos, ivec2(0));
    pos = Max(caa, pos);

    bb = Min(bb, cbb - ivec2(1) - pos);

    if (!(bb >= ivec2(0)))
        return;
    screen->AddDirty(pos, pos + bb);

  int ye=aa.y+bb.y;
  int xe=aa.x+bb.x;

  uint8_t *isl=im->scan_line(aa.y)+aa.x;
  uint8_t *ssl=screen->scan_line(y)+x;
  int iw=im->Size().x,sw=screen->Size().x;

  for (int iy=aa.y; iy<ye; iy++,y++,isl+=iw,ssl+=sw)
  {
    uint8_t *s=ssl,*i=isl;
    for (int ix=aa.x; ix<xe; ix++,s++,i++)
    {
      if (*i)
        *s=*i;
      else *s=table[*s];
    }
  }
}


/*
void transp_put(image *im, image *screen, uint8_t *table, int x, int y)
{
  int cx1, cy1, cx2, cy2;
  screen->GetClip(cx1, cy1, cx2, cy2);
  int xs=0,ys=0,xl=im->width(),yl=im->height();
  if (x<cx1)
  {
    int chop=cx1-x;
    xs+=chop;
    xl-=chop;
    x+=chop;
  }
  if (y<cy1)
  {
    int chop=cy1-y;
    ys+=chop;
    yl-=chop;
    y+=chop;
  }
  if (x + xl >= cx2)
    xl = cx2 - 1 - x;
  if (y + yl >= cy2)
    yl = cy2 - 1 - y;

  if (xl<0 || yl<0) return ;
  screen->AddDirty(x, y, x + xl - 1, y + yl);

  int ye=ys+yl;
  int xe=xs+xl;

  uint8_t *isl=im->scan_line(ys)+xs;
  uint8_t *ssl=screen->scan_line(y)+x;
  int iw=im->width(),sw=screen->width();

  for (int iy=ys; iy<ye; iy++,y++,isl+=iw,ssl+=sw)
  {
    uint8_t *s=ssl,*i=isl;
    for (int ix=xs; ix<xe; ix++,s++,i++)
      *s=table[((*i)<<8)|(*s)];
  }
}


*/
