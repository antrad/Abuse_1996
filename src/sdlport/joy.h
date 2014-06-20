/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __JOYSTICK_HPP_
#define __JOYSTICK_HPP_

int joy_init(int argc, char **argv); // returns 0 if no joystick is available
void joy_status(int &b1, int &b2, int &b3, int &xv, int &yv);
void joy_calibrate();

#endif
