/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __SPECACHE_HPP_
#define __SPECACHE_HPP_

#include "specs.h"

#include <string.h>

class spec_directory_cache
{
  class filename_node
  {
    public :
    filename_node *left,*right,*next;
    char *fn;
    spec_directory *sd;
    char *filename() { return fn; }
    filename_node(char const *filename, spec_directory *dir)
    {
      fn = strdup(filename);
      sd = dir;
      next = left = right = 0;
    }
    long size;
  } *fn_root,*fn_list;
  void clear(filename_node *f); // private recursive member
  long size;
  public :
  spec_directory *get_spec_directory(char const *filename, bFILE *fp=NULL);
  spec_directory_cache() { fn_root=0; size=0; }
  void clear();                             // frees up all allocated memory
  void load(bFILE *fp);
  void save(bFILE *fp);
  ~spec_directory_cache() { clear(); }
} ;

extern spec_directory_cache sd_cache;

#endif
