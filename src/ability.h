/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef ABILITIES_HPP_
#define ABILITIES_HPP_

enum ability
{    start_hp,
    start_accel,
    stop_accel,
        jump_xvel,
    jump_yvel,
    run_top_speed,
    jump_top_speed,

    tint_color,
    push_xrange,
    walk_top_speed                  // keep as last entry!
} ;

#define TOTAL_ABILITIES (walk_top_speed+1)
extern char const *ability_names[TOTAL_ABILITIES];
long get_ability(int who, ability a);
long get_ability_default(ability a);

#endif


