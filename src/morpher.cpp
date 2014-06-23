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

#include "morpher.h"
#include "game.h"
#include "objects.h"
#include "view.h"

void morph_char::draw(game_object *who, view *v)
{
    if (fleft)
    {
        ivec2 pos = the_game->GameToMouse(ivec2(who->x - (cx >> 16),
                                                who->y - (cy >> 16)), v);
        mor->show(main_screen, pos.x, pos.x, color_table, pal, 1000);
        cx += dcx;
        cy += dcy;
        fleft--;
    }
}



morph_char::morph_char(game_object *who, int to_type, void (*stat_fun)(int), int anneal, int frames)
{
  mor=NULL;
  CharacterType *t1=figures[who->otype],*t2=figures[to_type];
  if (!t1->has_sequence(morph_pose) || t1->morph_mask<0 ||
      !t2->has_sequence(morph_pose) || t2->morph_mask<0)
    fleft=0;
  else
  {
    if (anneal==-1)
    {
      switch (morph_detail)
      {
    case HIGH_DETAIL :
    { anneal=30; } break;
    case MEDIUM_DETAIL :
    { anneal=15; } break;
    case LOW_DETAIL :
    { anneal=8; } break;
    case POOR_DETAIL :
    { anneal=3; } break;
      }
    }

    fleft=frames;
    TransImage *h1=new TransImage(cache.img(t1->morph_mask),"morph tmp"),
                *h2=new TransImage(cache.img(t2->morph_mask),"morph tmp");
    super_morph *sm=new super_morph(h1,h2,anneal,stat_fun);
    if (sm->t)
    {
      delete h1;
      delete h2;
      figure *f1=t1->get_sequence(morph_pose)->get_figure(0),
      *f2=t2->get_sequence(morph_pose)->get_figure(0);
      image *i1=f1->forward->ToImage(),
      *i2=f2->forward->ToImage();

      mor=new smorph_player(sm,pal,i1,i2,fleft,who->direction);
      delete i2;
      delete i1;
      delete sm;

      if (who->direction>0)
      {
    cx=((int)f1->xcfg)<<16;
    dcx=(((int)f2->xcfg-(int)f1->xcfg)<<16)/(fleft-1);
      } else
      {
    cx=(mor->w-((int)f1->xcfg))<<16;
    dcx=((((int)f1->xcfg-(int)f2->xcfg))<<16)/(fleft-1);
      }
      cy=((int)f1->height()-1)<<16;
      dcy=((f2->height()-f1->height())<<16)/(fleft-1);
    } else
    {
      delete sm;
      fleft=0;
    }
  }
}











