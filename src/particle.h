/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __PARTICLE_HPP_
#define __PARTICLE_HPP_

#include "specs.h"
#include "image.h"

class view;

int defun_pseq(void *args);
void add_panim(int id, long x, long y, int dir);
void delete_panims();      // called by ~level
void draw_panims(view *v);
void tick_panims();
void free_pframes();
void ScatterLine(ivec2 p1, ivec2 p2, int c, int s);
void AScatterLine(ivec2 p1, ivec2 p2, int c1, int c2, int s);

struct part
{
  short x,y;
  uint8_t color;
} ;

class part_frame
{
  public :
  int t,x1,y1,x2,y2;
  part *data;
  part_frame(bFILE *fp);
  void draw(image *screen, int x, int y, int dir);
  ~part_frame();
} ;

class part_sequence
{
  public :
  int tframes;
  int *frames;  // array of id's
  part_sequence(void *args);
  ~part_sequence() { if (tframes) free(frames); }
} ;

class part_animation
{
  public :
  part_animation *next;
  part_sequence *seq;
  int frame,dir;
  long x,y;
  part_animation(part_sequence *s, long X, long Y, int Dir, part_animation *Next)
  { x=X; y=Y; seq=s; next=Next; frame=0; dir=Dir; }
} ;

#endif

