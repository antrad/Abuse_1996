/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __MOUSE_HPP_
#define __MOUSE_HPP_
#include "image.h"
#include "sprite.h"

class JCMouse
{
  int here,but;
  sprite *sp;
  image *screen;
  int lx,ly,lbut,mx,my;
  int cx,cy;                       // center of mouse cursor
public :
  JCMouse(image *Screen, palette *pal);
  void set_shape(image *im, int centerx=0, int centery=0);
  void update(int newx=-1, int newy=-1, int new_but=-1);
  void set_shape(image *im) { if (here) sp->change_visual(im); }
  int x() { if (here) return mx; else return 0; }
  int y() { if (here) return my; else return 0; }
  int drawx() { return mx-cx; }
  int drawy() { return my-cy; }
  int lastx() { if (here) return lx; else return 0; }
  int lasty() { if (here) return ly; else return 0; }
  int last_button() { if (here) return lbut; else return 0; }
  int button() { return but; }
  int exsist() { return here; }
  sprite *mouse_sprite() { return sp; }
  void set_position(int new_mx, int new_my);
  ~JCMouse();

#ifdef __POWERPC__
    int set_button(int b) { return (but = b); }
#endif
} ;

#endif

