/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __SUPER_MORPH_HPP__
#define __SUPER_MORPH_HPP__

#include "transimage.h"

class super_morph
{
public :
  int t;
  unsigned char *movers;
  int w,h;
  super_morph(TransImage *h1, TransImage *h2, int aneal_steps, void (*stat_fun)(int));
  ~super_morph() { if (t) free(movers); }
} ;


struct stepper
{
  long x,y,r,g,b,dx,dy,dr,dg,db;
} ;

class smorph_player
{
  stepper *steps;
  unsigned char *hole;
public :
  int w,h,f_left,t;
  smorph_player(super_morph *m, palette *pal, image *i1, image *i2, int frames, int dir);
  int show(image *screen, int x, int y, ColorFilter *fil, palette *pal, int blur_threshold);
  ~smorph_player() { free(hole); free(steps);  }
} ;


#endif
