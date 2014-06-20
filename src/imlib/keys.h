/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __KEYS_HPP_
#define __KEYS_HPP_

#define JK_BACKSPACE 8
#define JK_TAB      9
#define JK_ENTER    13
#define JK_ESC      27
#define JK_SPACE    32

#define JK_UP       256
#define JK_DOWN     257
#define JK_LEFT     258
#define JK_RIGHT    259
#define JK_CTRL_L   260
#define JK_CTRL_R   261
#define JK_ALT_L    262
#define JK_ALT_R    263
#define JK_SHIFT_L  264
#define JK_SHIFT_R  265
#define JK_CAPS     266
#define JK_NUM_LOCK 267
#define JK_HOME     268
#define JK_END      269
#define JK_DEL      270
#define JK_F1       271
#define JK_F2       272
#define JK_F3       273
#define JK_F4       274
#define JK_F5       275
#define JK_F6       276
#define JK_F7       277
#define JK_F8       278
#define JK_F9       279
#define JK_F10      280
#define JK_INSERT   281
#define JK_PAGEUP   282
#define JK_PAGEDOWN 283
#define JK_COMMAND  284
#define JK_MAX_KEY  284

// returns a ASCII string describing a key, i.e. "Up Arrow"
void key_name(int key, char *buffer);

// returns a value describing a key name
int key_value(char const *buffer);

#endif







