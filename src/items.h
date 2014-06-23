/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __ITEMS_HPP__
#define __ITEMS_HPP__
#include "image.h"
#include "transimage.h"
#include "specs.h"
#include "points.h"
#include <stdio.h>
#include <stdlib.h>

#define AUTOTILE_WIDTH 6
#define AUTOTILE_HEIGHT 3

class boundary : public point_list      //  a list of points with
{
public :
  boundary(bFILE *fp,char const *er_name);
  uint8_t *inside;     // tells which side of the line is on the inside
  boundary(boundary *p);      // flips the *inside list
  ~boundary() { if (tot) free(inside); }
} ;

class backtile
{
public :
  uint16_t next;
  image *im;
  backtile(spec_entry *e, bFILE *fp);
  backtile(bFILE *fp);
  int32_t size() { ivec2 s = im->Size(); return 2 + 4 + s.x * s.y; }
  ~backtile() { delete im; }
} ;

class foretile
{
public :
  TransImage *im;
  uint16_t next;
  uint8_t damage;
  uint8_t ylevel;            // for fast intersections, this is the y level offset for the ground
                           // if ground is not level this is 255
  boundary *points;

  image *micro_image;

  foretile(bFILE *fp);
  int32_t size() { return im->Size().x*im->Size().y+4+2+1+points->size(); }
  ~foretile() { delete im; delete points; delete micro_image; }
} ;

class figure
{
public :
  TransImage *forward,*backward;
  uint8_t hit_damage,xcfg;
  int8_t advance;
  point_list *hit;
  boundary *f_damage,*b_damage;
  size_t MemUsage();

  figure(bFILE *fp, int type);
  int width() { return forward->Size().x; }
  int height() { return forward->Size().y; }

  ~figure() { delete forward; delete backward; delete hit;
              delete f_damage; delete b_damage; }
} ;

class char_tint
{
  public :
  uint8_t data[256];
  ~char_tint() { ; }
  char_tint(bFILE *fp);               // should be a palette entry
} ;

#endif
















