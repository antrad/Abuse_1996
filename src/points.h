/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __POINTS_HPP_
#define __POINTS_HPP_

#include <stdio.h>
#include <stdlib.h>

#include "specs.h"

class point_list
{
public :
  unsigned char tot;
  unsigned char *data;
  point_list(unsigned char how_many, unsigned char *Data);
  point_list() { tot=0; data=NULL; }
  point_list(bFILE *fp);
  void save(bFILE *fp);
  long size() { return 1+2*tot; }
  point_list *copy() { return new point_list(tot,data); }
  ~point_list() { if (tot) { free(data); } }
} ;

#endif


