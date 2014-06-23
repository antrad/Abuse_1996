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

#include "extend.h"
#include "view.h"
#include "objects.h"
#include "lisp.h"

/*



  Simple object             (power ups, non-moving objects)
    int32_t x,y;
    int8_t direction;
    uint16_t otype,state
    uint16_t current_frame;
    extension *


  Moving object             (simple lisp controlled characters)
     uint8_t flags;
     int32_t xvel,yvel,xacel,yacel;
     uint8_t fx,fy,fxvel,fyvel,fxacel,fyacel,aitype;
     uint16_t aistate,aistate_time;
     uint16_t hp,mp,
     extension *


  Complex objects          (can controll lights, other characters, and have a neural net ai)
    uint8_t tobjs,tlights;
    object_list *
    light_list *
    nnet_info *
    int8_t fade_dir, frame_dir;
    uint8_t fade_count,fade_max;
    morph_char *morph_status;


*/

void simple_object::add_light(light_source *ls)
{
  if (!ls) return ;
  ls->known=1;
  for (int i=0; i<tlights; i++) if (lights[i]==ls) return;
  tlights++;
  lights=(light_source **)realloc(lights,sizeof(light_source *)*tlights);
  lights[tlights-1]=ls;
}

void simple_object::add_object(game_object *o)
{
  if (!o) return ;
  for (int i=0; i<tobjs; i++) if (objs[i]==o) return;
  o->set_flags(o->flags()|KNOWN_FLAG);
  if(_team != -1)
    o->set_team(_team);
  if(_tint != -1)
    o->set_tint(_tint);
  tobjs++;
  objs=(game_object **)realloc(objs,sizeof(game_object *)*tobjs);
  objs[tobjs-1]=o;
}


void simple_object::remove_light(light_source *ls)
{
  for (int i=0; i<tlights; i++)
  {
    if (lights[i]==ls)
    {
      tlights--;
      for (int j=i; j<tlights; j++)     // don't even think about it :)
        lights[j]=lights[j+1];
      lights=(light_source **)realloc(lights,sizeof(light_source *)*tlights);
      return ;
    }
  }
}

void simple_object::remove_object(game_object *o)
{
  for (int i=0; i<tobjs; i++)
  {
    if (objs[i]==o)
    {
      tobjs--;
      for (int j=i; j<tobjs; j++)     // don't even think about it :)
        objs[j]=objs[j+1];
      objs=(game_object **)realloc(objs,sizeof(game_object *)*tobjs);
      return ;
    }
  }
}


simple_object::simple_object()
{

  x=y=0;
  direction=1;
  otype=0;
  state=stopped;
  current_frame=0;

  Fade_dir=0;
  Fade_count=0;
  Fade_max=16;


  tobjs=tlights=0;
  objs=NULL;
  lights=NULL;
  Frame_dir=1;
  mc=NULL;
  Controller=NULL;

  Flags=0;
  Xvel=Yvel=Xacel=Yacel=0;
  Fx=Fy=Fxvel=Fyvel=Fxacel=Fyacel=Aitype=0;
  Aistate=Aistate_time=0;
  Hp=Mp=Fmp=0;
  _tint = -1;
  _team = -1;
  grav_on=1;
  targetable_on=1;
}



void simple_object::set_morph_status(morph_char *Mc)
{
  mc=Mc;
}

void simple_object::clean_up()
{
  if (tlights) free(lights);
  if (tobjs)   free(objs);
  if (Controller)
    Controller->m_focus=NULL;
}


simple_object default_simple;


