/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __EVENT_HPP_
#define __EVENT_HPP_
#define EV_MOUSE_MOVE     1
#define EV_MOUSE_BUTTON   2
#define EV_KEY            4
#define EV_KEY_SPECIAL    8
#define EV_REDRAW        16
#define EV_SPURIOUS      32
#define EV_RESIZE        64
#define EV_KEYRELEASE   128
#define EV_CLOSE_WINDOW 256
#define EV_DRAG_WINDOW  512
#define EV_MESSAGE     1024

#define LEFT_BUTTON    1
#define RIGHT_BUTTON   2
#define MIDDLE_BUTTON  4
#include "keys.h"
#include "sprite.h"
#include "mouse.h"

class Jwindow;

class event : public linked_node
{
public :
  int                             type;
  struct { int x,y; }             mouse_move;
  int                             mouse_button;
  int                             key;
  struct { char alt,ctrl,shift; } key_special;
  struct { int x1,y1,x2,y2;
       void *start; }         redraw;
  Jwindow                        *window;      // NULL is root
  struct { int x,y; }             window_position;
  struct { int id; char *data; }  message;
  event(int id, char *data) { type=EV_MESSAGE; message.id=id; message.data=data; }
  event() { type=EV_SPURIOUS; }
} ;

class event_handler
{
  sprite_controller sc;
  int mhere,ewaiting,last_keystat,last_key;
  int get_key_flags();
  linked_list events;
public :
  JCMouse *mouse;
  sprite *mouse_sprite() { return mouse->mouse_sprite(); }
  event_handler(image *screen, palette *pal);
  int event_waiting();
  void get_event(event &ev);
  void add_redraw(int X1, int Y1, int X2, int Y2, void *Start);
  void mouse_status(int &x, int &y, int &button)
  { if (mouse)
    {
      x=mouse->x(); y=mouse->y(); button=mouse->button();
    } else x=y=button=0;
  }
  void push_event(event *ev) { events.add_end(ev); }
  void flush_screen();
  int has_mouse() { return mouse->exsist(); }
  void set_mouse_shape(image *im, int centerx, int centery) { mouse->set_shape(im,-centerx,-centery); }
  void set_mouse_position(int mx, int my) { if (mouse) mouse->set_position(mx,my); }
  ~event_handler();
} ;
#endif

