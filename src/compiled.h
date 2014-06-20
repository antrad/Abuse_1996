/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __COMPILED_HPP_
#define __COMPILED_HPP_

extern int32_t S_fall_start,S_falling,S_landing,S_pounce_wait,
            S_turn_around,S_fire_wait,S_ceil_fire,S_top_walk,
            S_blown_back_dead,S_jump_up,S_hiding,S_weapon_fire,
        S_hanging,S_blocking,S_rotate,S_climbing,S_climb_off,
        S_fly_stopped,S_fast_stopped,S_fast_running,S_fly_running,
        S_fly_start_run_jump,S_fly_run_jump,S_fly_run_jump_fall,S_fly_end_run_jump,
        S_fast_start_run_jump,S_fast_run_jump,S_fast_run_jump_fall,S_fast_end_run_jump,
        S_bright_tint,S_climb_on,

        S_ALAND_SND,S_ASCREAM_SND,S_ASLASH_SND,
        S_ROCKET_TOP,S_BFG_TOP,S_GRENADE_TOP,S_DFRIS_TOP,S_FIREBOMB_TOP,S_ANT_ROOF,
        S_MGUN_TOP,S_CLOUD,S_HIDDEN_ANT,
        S_health_image,S_fly_image,S_fast_image,
        S_sneaky_image,S_EXPLODE5,S_EXPLODE3,S_ROCKET,
        S_TRACK_GUN,S_SPRAY_GUN,S_LPING_SND,S_FLY_SND,S_SPEED_SND,S_SCARE_SND,
        S_LOW_HEALTH_SND,S_BUTTON_PRESS_SND,
        S_LINK_SND,S_DELETE_SND;



void compiled_init();
void compiled_uninit();

extern uint8_t *bad_guy_array;  // list flaging each character saying they are a bad bug or not
                                // mainly used by the rocket to find targets

#endif
