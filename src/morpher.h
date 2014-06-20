/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __MORPHER_HPP_
#define __MORPHER_HPP_
#include "supmorph.h"
#include "configuration.h"

extern int morph_detail;

class view;

class game_object;

class morph_char
{
  long cx,dcx,cy,dcy;                   // center of gravity
  smorph_player *mor;
  int end_type,fleft;
public :
  morph_char(game_object *who, int to_type, void (*stat_fun)(int), int anneal, int frames);
  void draw(game_object *who, view *v);
  int frames_left() { return fleft; }
  virtual ~morph_char() { if (mor) delete mor; }
} ;


#endif


