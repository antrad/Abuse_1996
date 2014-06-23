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

#include <cstring>

#include "lisp.h"

using namespace std;

extern int total_objects;

uint8_t *bad_guy_array=NULL;  // list flaging each character saying they are a bad bug or not
                              // mainly used by the rocket to find targets

int32_t S_fall_start,S_falling,S_landing,S_pounce_wait,
     S_turn_around,S_fire_wait,S_ceil_fire,S_top_walk,
     S_blown_back_dead,S_jump_up,S_hiding,S_weapon_fire,
     S_hanging,S_blocking,S_rotate,S_climbing,S_climb_off,
     S_fly_stopped,S_fast_stopped,S_fast_running,S_fly_running,
     S_fly_start_run_jump,S_fly_run_jump,S_fly_run_jump_fall,S_fly_end_run_jump,
     S_fast_start_run_jump,S_fast_run_jump,S_fast_run_jump_fall,S_fast_end_run_jump,
     S_bright_tint,S_climb_on,
     S_ASCREAM_SND,S_ALAND_SND,S_ASLASH_SND,S_ROCKET_TOP,S_BFG_TOP,
     S_GRENADE_TOP,S_FIREBOMB_TOP,S_DFRIS_TOP,S_ANT_ROOF,
     S_MGUN_TOP,S_CLOUD,S_HIDDEN_ANT,
     S_health_image,S_fly_image,S_fast_image,
     S_sneaky_image,S_EXPLODE3,S_EXPLODE5,S_ROCKET,S_SCARE_SND,
     S_TRACK_GUN,S_SPRAY_GUN,S_LPING_SND,S_FLY_SND,S_SPEED_SND,S_LOW_HEALTH_SND,
     S_BUTTON_PRESS_SND,
     S_LINK_SND,S_DELETE_SND;

int compile_error=0;

static int32_t c_state(char const *name)
{
  void *sym = LSymbol::Find(name);
  if (sym)
  {
    if (item_type(((LSymbol *)sym)->m_value)!=L_NUMBER)
      compile_error=1;
    else
      return lnumber_value(((LSymbol *)sym)->m_value);
  } else compile_error=1;
  return 0;
}

extern int last_save_game_number;

void compiled_init()
{
  S_fall_start=  c_state("fall_start");
  S_falling=     c_state("falling");
  S_landing=     c_state("landing");
  S_pounce_wait= c_state("pounce_wait");
  S_turn_around= c_state("turn_around");
  S_fire_wait=   c_state("fire_wait");
  S_ceil_fire=   c_state("ceil_fire");
  S_top_walk=    c_state("top_walk");
  S_jump_up=     c_state("jump_up");
  S_rotate=      c_state("rotate");
  S_hiding=      c_state("hiding");
  S_weapon_fire= c_state("weapon_fire");
  S_climbing=    c_state("climbing");
  S_climb_off=   c_state("climb_off");
  S_blown_back_dead=c_state("blown_back_dead");
  S_hanging=     c_state("hanging");
  S_blocking=     c_state("blocking");

  S_fly_stopped= c_state("fly_stopped");
  S_fast_stopped=c_state("fast_stopped");
  S_fast_running=c_state("fast_running");
  S_fly_running= c_state("fly_running");

  S_fly_start_run_jump=     c_state("fly_start_run_jump");
  S_fly_run_jump=           c_state("fly_run_jump");
  S_fly_run_jump_fall=      c_state("fly_run_jump_fall");
  S_fly_end_run_jump=       c_state("fly_end_run_jump");

  S_fast_start_run_jump=    c_state("fast_start_run_jump");
  S_fast_run_jump=          c_state("fast_run_jump");
  S_fast_run_jump_fall=     c_state("fast_run_jump_fall");
  S_fast_end_run_jump=      c_state("fast_end_run_jump");
  S_bright_tint=            c_state("bright_tint");
  S_climb_on=               c_state("climb_on");

  S_ALAND_SND=   c_state("ALAND_SND");
  S_ASCREAM_SND= c_state("ASCREAM_SND");
  S_ASLASH_SND=  c_state("ASLASH_SND");
  S_GRENADE_TOP= c_state("GRENADE_TOP");
  S_FIREBOMB_TOP=c_state("FIREBOMB_TOP");
  S_ANT_ROOF=    c_state("ANT_ROOF");
  S_MGUN_TOP=    c_state("MGUN_TOP");
  S_DFRIS_TOP=   c_state("DFRIS_TOP");
  S_ROCKET_TOP=  c_state("ROCKET_TOP");
  S_BFG_TOP=     c_state("BFG_TOP");
  S_CLOUD=       c_state("CLOUD");
  S_HIDDEN_ANT=  c_state("HIDDEN_ANT");
  S_health_image=c_state("health_image");
  S_fly_image=   c_state("fly_image");
  S_sneaky_image=c_state("sneaky_image");
  S_fast_image  =c_state("fast_image");
  S_EXPLODE5=    c_state("EXPLODE5");
  S_EXPLODE3=    c_state("EXPLODE3");
  S_ROCKET=      c_state("ROCKET");
  S_TRACK_GUN=   c_state("TRACK_GUN");
  S_SPRAY_GUN=   c_state("SPRAY_GUN");
  S_LPING_SND=   c_state("LPING_SND");
  S_FLY_SND=     c_state("FLY_SND");
  S_SPEED_SND=   c_state("SPEED_SND");
  S_LOW_HEALTH_SND=c_state("LOW_HEALTH_SND");
  S_SCARE_SND=   c_state("SCARE_SND");
  S_BUTTON_PRESS_SND=c_state("BUTTON_PRESS_SND");
  S_LINK_SND=    c_state("LINK_OBJECT_SND");
  S_DELETE_SND=  c_state("DEL_OBJECT_SND");

  void *b = LSymbol::FindOrCreate("bad_guy_list");
  if (b && DEFINEDP(symbol_value(b)))
  {
    b=symbol_value(b);
    bad_guy_array=(uint8_t *)malloc(total_objects);
    memset(bad_guy_array,0,total_objects);
    while (b)
    {
      int32_t x=lnumber_value(CAR(b));
      if (x>=0 && x<total_objects)
        bad_guy_array[x]=1;
      else { lbreak("objetc number out of range %d\n",x); }
      b=lcdr(b);
    }
  }

  void *v = LSymbol::FindOrCreate("last_save_game")->GetValue();
  if (DEFINEDP(v))
    last_save_game_number=lnumber_value(v);
  else last_save_game_number=0;
}




void compiled_uninit()
{
  if (bad_guy_array)
    free(bad_guy_array);
}
