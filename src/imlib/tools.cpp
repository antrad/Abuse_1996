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

#include "tools.h"



tool_picker::~tool_picker()
{ delete old_pal;
  delete map;
  for (int i=0; i<total_icons; i++)
    delete icons[i];                   // delete visual object, which should be a "shell"
}

void tool_picker::remap(palette *pal, image *screen)
{
  delete map;
  map=new Filter(old_pal,pal);
  draw_first(screen);
}

tool_picker::tool_picker(int X, int Y, int ID,
          int show_h, visual_object **Icons, int *Ids, int total_ic,
             palette *icon_palette, palette *pal, ifield *Next) :
  spicker(X,Y,ID,show_h,1,1,0,Next)
{
  iw=ih=0;
  icons=Icons;
  ids=Ids;
  total_icons=total_ic;
  for (int i=0; i<total_ic; i++)
  {
    if (icons[i]->width()>iw) iw=icons[i]->width();
    if (icons[i]->height()>ih) ih=icons[i]->height();
  }
  map=new Filter(icon_palette,pal);
  old_pal=icon_palette->copy();
  reconfigure();
}

void tool_picker::draw_item(image *screen, int x, int y, int num, int active)
{
    screen->Bar(ivec2(x, y), ivec2(x + iw - 1, y + ih - 1),
                active ? wm->bright_color() : wm->black());
    icons[num]->draw(screen, x, y, map);
}

