/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __COP_HPP_
#define __COP_HPP_

// functions defined for the main player,  these were translated because they
// are called every tick and they were getting slow/complicated

void *top_ai();
void *laser_ufun(void *args);
void *top_ufun(void *args);
void *plaser_ufun(void *args);
void *player_rocket_ufun(void *args);
void *lsaber_ufun(void *args);
void *cop_mover(int xm, int ym, int but);
void *sgun_ai();
void *ladder_ai();
void *top_draw();
void *bottom_draw();
void *mover_ai();
void *respawn_ai();
void *score_draw();
void *show_kills();

#endif
