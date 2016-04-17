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

/* Q: Why are these powers of 2? They're never ORed together... */
#define EV_MOUSE_MOVE     1
#define EV_MOUSE_BUTTON   2
#define EV_KEY            4
/*#define EV_KEY_SPECIAL    8 UNUSED
 #define EV_REDRAW        16 UNUSED */
#define EV_SPURIOUS      32
/* RESIZE is effectively unused (it can never be generated) */
#define EV_RESIZE        64
#define EV_KEYRELEASE   128
#define EV_CLOSE_WINDOW 256
/* DRAG_WINDOW is effectively unused (it CAN be generated, but is never processed) */
#define EV_DRAG_WINDOW  512
#define EV_MESSAGE     1024

#define LEFT_BUTTON    1
#define RIGHT_BUTTON   2
#define MIDDLE_BUTTON  4

#include "keys.h"
#include "sprite.h"

class Jwindow;

class Event : public linked_node
{
public:
    Event()
    {
        type = EV_SPURIOUS;
    }

    Event(int id, char *data)
    {
        type = EV_MESSAGE;
        message.id = id;
        message.data = data;
    }

    int type;
    ivec2 mouse_move;
    int mouse_button, key;

    struct { char alt, ctrl, shift; } key_special;

    Jwindow *window;      // NULL is root
    ivec2 window_position;
    struct { int id; char *data; } message;
};

class EventHandler
{
public:
    EventHandler(image *screen, palette *pal);
    ~EventHandler();

    void Push(Event *ev)
    {
        m_events.add_end(ev);
    }

    void SysInit();
    void SysWarpMouse(ivec2 pos);
    void SysEvent(Event &ev);

    int IsPending();
    void Get(Event &ev);
    void flush_screen();

    int has_mouse() { return 1; }
    void SetMouseShape(image *im, ivec2 center)
    {
        m_sprite->SetVisual(im, 1);
        m_center = center;
    }
    void SetMousePos(ivec2 pos)
    {
        m_pos = ivec2(Min(Max(pos.x, 0), m_screen->Size().x - 1),
                      Min(Max(pos.y, 0), m_screen->Size().y - 1));
        SysWarpMouse(m_pos);
    }
	//AR
	ivec2 GetMousePos()
    {
         return this->m_pos;
    }
    void SetIgnoreWheelEvents(bool ignore)
    {
        m_ignore_wheel_events = ignore;
    }
    void SetRightStickCenter(int x, int y)
    {
        m_right_stick_x = x;
        m_right_stick_y = y;
    }
    void SetRightStickMouse()
    {
        m_right_stick_x = m_right_stick_y = -1;
    }

private:
    linked_list m_events;
    int m_pending, last_key;
    bool m_ignore_wheel_events;
    // "Dead zone" before motion of a stick "counts".
    // Maximum stick values are 0x7FFF, currently I've
    // arbitrarily set this to 1/4th.
    int m_dead_zone;//AR (int m_dead_zone = 0x2000;)
    // Scale amount for the right stick when moving the mouse. The range is
    // -0x7FFF to 0x7FFF, or -32767 to 32767. The default means it will move
    // a maximum of 3 pixels per tick.
    int m_right_stick_scale;//AR (int m_right_stick_scale = 0x2000;)
    // Scale amount for the right stick when it's player-locked.
    // 0x400 gives a range of -31 to 31.
    int m_right_stick_player_scale;//AR (int m_right_stick_player_scale = 0x400;)
    int m_right_stick_x, m_right_stick_y;

    image *m_screen;

protected:
    /* Mouse information */
    Sprite *m_sprite;
    ivec2 m_pos, m_center;
    int m_button;
};

#endif
