/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 2001 Anthony Kruize <trandor@labyrinth.net.au>
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *  Copyright (c) 2016 Antonio Radojkovic <antonior.software@gmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston MA 02110-1301, USA.
 */

#if defined HAVE_CONFIG_H
#   include "config.h"
#endif

#include "SDL.h"

#include "common.h"

#include "image.h"
#include "palette.h"
#include "video.h"
#include "event.h"
#include "timing.h"
#include "sprite.h"
#include "game.h"
#include "setup.h"

extern SDL_Window *window;
extern SDL_Surface *surface;

extern Settings settings;
extern int get_key_binding(char const *dir, int i);
extern std::string get_ctr_binding(std::string c);

extern int mouse_xpad, mouse_ypad, mouse_xscale, mouse_yscale;
short mouse_buttons[5] = { 0, 0, 0, 0, 0 };
// From setup.cpp:
void video_change_settings(int scale_add, bool toggle_fullscreen);
void calculate_mouse_scaling(void);

//AR on my brand new Xbox360 controller using the D-pad would trigger left stick movement events... best controller of all time they say...sigh
//so I disable it if the user uses a D-pad, and enable it if the user uses the stick and passes the dead zone
bool use_left_stick = false;

void EventHandler::SysInit()
{
    // Ignore activate events
    // This event is gone in SDL2, should we be ignoring the replacement? Dunno
    //SDL_EventState(SDL_ACTIVEEVENT, SDL_IGNORE);
}

void EventHandler::SysWarpMouse(ivec2 pos)
{
    // This should take into account mouse scaling.
    pos.x = ((pos.x * mouse_xscale + 0x8000) >> 16) + mouse_xpad;
    pos.y = ((pos.y * mouse_yscale + 0x8000) >> 16) + mouse_ypad;
	//AR this repositions the system mouse based on in game values, so I turned it off for controller aiming
    SDL_WarpMouseInWindow(window, pos.x, pos.y);
}

//
// IsPending()
// Are there any events in the queue?
//
int EventHandler::IsPending()
{
    if (!m_pending && SDL_PollEvent(NULL))
        m_pending = 1;

    return m_pending;
}

//
// Get and handle waiting events
//
void EventHandler::SysEvent(Event &ev)
{
    // No more events
    m_pending = 0;

    // NOTE : that the mouse status should be known
    // even if another event has occurred.
	
	ev.mouse_move.x = m_pos.x;
	ev.mouse_move.y = m_pos.y;	
    ev.mouse_button = m_button;

    // Gather next event
    SDL_Event sdlev;
    if (!SDL_PollEvent(&sdlev))
        return; // This should not happen

	// Sort the mouse out
	int x, y;
	uint8_t buttons = SDL_GetMouseState(&x, &y);

	// Remove any padding SDL may have added
	x -= mouse_xpad;
	if (x < 0) x = 0;
	y -= mouse_ypad;
	if (y < 0) y = 0;

	x = Min((x << 16) / mouse_xscale, main_screen->Size().x - 1);
	y = Min((y << 16) / mouse_yscale, main_screen->Size().y - 1);

	ev.mouse_move.x = x;
	ev.mouse_move.y = y;
	ev.type = EV_MOUSE_MOVE;

	//AR God knows where and what player uses as a final value to aim, m_pos or ev.mouse_move !?
	//this prevents flickering when aiming with a controller
	//we need to disable this if we are in the save game state in game, so we can use the mouse
	if(settings.ctr_aim==1 && settings.in_game && the_game->ar_state!=AR_LOADSAVE)
	{
		ev.mouse_move.x = m_pos.x;
		ev.mouse_move.y = m_pos.y;
	}
	
	// Left button
	if((buttons & SDL_BUTTON(1)) && !mouse_buttons[1])
	{
		ev.type = EV_MOUSE_BUTTON;
		mouse_buttons[1] = !mouse_buttons[1];
        ev.mouse_button |= LEFT_BUTTON;
    }
    else if(!(buttons & SDL_BUTTON(1)) && mouse_buttons[1])
    {
        ev.type = EV_MOUSE_BUTTON;
        mouse_buttons[1] = !mouse_buttons[1];
        ev.mouse_button &= (0xff - LEFT_BUTTON);
    }

    // Middle button
    if((buttons & SDL_BUTTON(2)) && !mouse_buttons[2])
    {
        ev.type = EV_MOUSE_BUTTON;
        mouse_buttons[2] = !mouse_buttons[2];
        ev.mouse_button |= LEFT_BUTTON;
        ev.mouse_button |= RIGHT_BUTTON;
    }
    else if(!(buttons & SDL_BUTTON(2)) && mouse_buttons[2])
    {
        ev.type = EV_MOUSE_BUTTON;
        mouse_buttons[2] = !mouse_buttons[2];
        ev.mouse_button &= (0xff - LEFT_BUTTON);
        ev.mouse_button &= (0xff - RIGHT_BUTTON);
    }

    // Right button
    if((buttons & SDL_BUTTON(3)) && !mouse_buttons[3])
    {
        ev.type = EV_MOUSE_BUTTON;
        mouse_buttons[3] = !mouse_buttons[3];
        ev.mouse_button |= RIGHT_BUTTON;
    }
    else if(!(buttons & SDL_BUTTON(3)) && mouse_buttons[3])
    {
        ev.type = EV_MOUSE_BUTTON;
        mouse_buttons[3] = !mouse_buttons[3];
        ev.mouse_button &= (0xff - RIGHT_BUTTON);
    }

    m_pos = ivec2(ev.mouse_move.x, ev.mouse_move.y);
    m_button = ev.mouse_button;

    // Sort out other kinds of events
    switch(sdlev.type)
    {
    case SDL_QUIT:
        exit(0);
        break;
    case SDL_WINDOWEVENT:
        switch (sdlev.window.event)
        {
        case SDL_WINDOWEVENT_RESIZED:
        case SDL_WINDOWEVENT_MAXIMIZED:
        case SDL_WINDOWEVENT_RESTORED:
        case SDL_WINDOWEVENT_MINIMIZED:
            // Recalculate mouse scaling and padding. Note that we may end up
            // double-doing this, but whatever. Who cares.
            calculate_mouse_scaling();
            break;
        }
    case SDL_MOUSEWHEEL:
        if (m_ignore_wheel_events)
            break;
        // Conceptually this can be in multiple directions, so use left/right
        // first because those match the bars on the button
        if (sdlev.wheel.x < 0)
        {
            ev.key = get_key_binding("b4", 0);
            ev.type = EV_KEY;
        }
        else if (sdlev.wheel.x > 0)
        {
            ev.key = get_key_binding("b3", 0);
            ev.type = EV_KEY;
        }
        else if (sdlev.wheel.y < 0)
        {
            ev.key = get_key_binding("b4", 0);
            ev.type = EV_KEY;
        }
        else if (sdlev.wheel.y > 0)
        {
            ev.key = get_key_binding("b3", 0);
            ev.type = EV_KEY;
        }
        if (ev.type == EV_KEY)
        {
            // We also need to immediately queue a "release" event or this will
            // be stuck down forever.
            Event *release_event = new Event();
            release_event->key = ev.key;
            release_event->type = EV_KEYRELEASE;
            Push(release_event);
        }
        break;
    case SDL_MOUSEBUTTONUP:
        // These were the old mouse wheel handlers, but honestly, using
        // B4 and B5 for weapon switching works.
        switch(sdlev.button.button)
        {
        case 4:        // Mouse wheel goes up...
            ev.key = get_key_binding("b4", 0);
            ev.type = EV_KEYRELEASE;
            break;
        case 5:        // Mouse wheel goes down...
            ev.key = get_key_binding("b3", 0);
            ev.type = EV_KEYRELEASE;
            break;
        }
        break;
    case SDL_MOUSEBUTTONDOWN:
        switch(sdlev.button.button)
        {
        case 4:        // Mouse wheel goes up...
            ev.key = get_key_binding("b4", 0);
            ev.type = EV_KEY;
            break;
        case 5:        // Mouse wheel goes down...
            ev.key = get_key_binding("b3", 0);
            ev.type = EV_KEY;
            break;
        }
        break;
    case SDL_KEYDOWN:
    case SDL_KEYUP:
        // Default to EV_SPURIOUS
        ev.key = EV_SPURIOUS;

        if(sdlev.type == SDL_KEYDOWN) ev.type = EV_KEY;        
        else ev.type = EV_KEYRELEASE;

        switch(sdlev.key.keysym.sym)
        {
        case SDLK_DOWN:         ev.key = JK_DOWN; break;
        case SDLK_UP:           ev.key = JK_UP; break;
        case SDLK_LEFT:         ev.key = JK_LEFT; break;
        case SDLK_RIGHT:        ev.key = JK_RIGHT; break;
        case SDLK_LCTRL:        ev.key = JK_CTRL_L; break;
        case SDLK_RCTRL:        ev.key = JK_CTRL_R; break;
        case SDLK_LALT:         ev.key = JK_ALT_L; break;
        case SDLK_RALT:         ev.key = JK_ALT_R; break;
        case SDLK_LSHIFT:       ev.key = JK_SHIFT_L; break;
        case SDLK_RSHIFT:       ev.key = JK_SHIFT_R; break;
        case SDLK_NUMLOCKCLEAR: ev.key = JK_NUM_LOCK; break;
        case SDLK_HOME:         ev.key = JK_HOME; break;
        case SDLK_END:          ev.key = JK_END; break;
        case SDLK_BACKSPACE:    ev.key = JK_BACKSPACE; break;
        case SDLK_TAB:          ev.key = JK_TAB; break;
        case SDLK_RETURN:       ev.key = JK_ENTER; break;
        case SDLK_SPACE:        ev.key = JK_SPACE; break;
        case SDLK_CAPSLOCK:     ev.key = JK_CAPS; break;
        case SDLK_ESCAPE:       ev.key = JK_ESC; break;
        case SDLK_F1:           ev.key = JK_F1; break;
        case SDLK_F2:           ev.key = JK_F2; break;
        case SDLK_F3:           ev.key = JK_F3; break;
        case SDLK_F4:           ev.key = JK_F4; break;
        case SDLK_F5:           ev.key = JK_F5; break;
        case SDLK_F6:           ev.key = JK_F6; break;
        case SDLK_F7:           ev.key = JK_F7; break;
		case SDLK_INSERT:       ev.key = JK_INSERT; break;
        case SDLK_KP_0:         ev.key = JK_INSERT; break;
        case SDLK_PAGEUP:       ev.key = JK_PAGEUP; break;
        case SDLK_PAGEDOWN:     ev.key = JK_PAGEDOWN; break;
        case SDLK_KP_8:         ev.key = JK_UP; break;
        case SDLK_KP_2:
		case SDLK_KP_5:			ev.key = JK_DOWN; break;
		case SDLK_KP_4:         ev.key = JK_LEFT; break;
		case SDLK_KP_6:         ev.key = JK_RIGHT; break;

			//change settings, only handle key down

		case SDLK_F8://AR toggle mouse scale			
			if(ev.type == EV_KEY)
			{
				if(settings.mouse_scale==0) settings.mouse_scale = 1;
				else settings.mouse_scale = 0;
			}
			ev.key = EV_SPURIOUS;
			break;

		case SDLK_F9://AR toggle controller aim
			if(ev.type == EV_KEY) settings.ctr_aim = !settings.ctr_aim;
			ev.key = EV_SPURIOUS;
			break;

		case SDLK_F10://toggle fullscreen, 			
			if(ev.type == EV_KEY) video_change_settings(0,true);
			ev.key = EV_SPURIOUS;
			break;

		case SDLK_F11://AR scale window up
			if(ev.type == EV_KEY) video_change_settings(1,false);
			ev.key = EV_SPURIOUS;
			break;

		case SDLK_F12://AR scale window down
			if(ev.type == EV_KEY) video_change_settings(-1,false);
			ev.key = EV_SPURIOUS;
			break;

		case SDLK_PRINTSCREEN://grab a screenshot
			if(ev.type == EV_KEY)
			{
				SDL_SaveBMP(surface, "screenshot.bmp");
				the_game->show_help("Screenshot saved to: screenshot.bmp.\n");
			}
			ev.key = EV_SPURIOUS;
			break;

		default:
			//AR this will crash in game.cpp calling key_down() which can go up to 64
			//so I set it to a random key which shouldn't do anything in the game
			if((int)sdlev.key.keysym.sym>JK_MAX_KEY) ev.key = JK_MAX_KEY;
			else ev.key = (int)sdlev.key.keysym.sym;

            // Need to handle the case of shift being pressed
            // There has to be a better way
            if((sdlev.key.keysym.mod & KMOD_SHIFT) != 0)
            {
                if(sdlev.key.keysym.sym >= SDLK_a &&
                    sdlev.key.keysym.sym <= SDLK_z)
                {
                    ev.key -= 32;
                }
                else if(sdlev.key.keysym.sym >= SDLK_1 &&
                         sdlev.key.keysym.sym <= SDLK_5)
                {
                    ev.key -= 16;
                }
                else
                {
                    switch(sdlev.key.keysym.sym)
                    {
                    case SDLK_6:
                        ev.key = SDLK_CARET; break;
                    case SDLK_7:
                    case SDLK_9:
                    case SDLK_0:
                        ev.key -= 17; break;
                    case SDLK_8:
                        ev.key = SDLK_ASTERISK; break;
                    case SDLK_MINUS:
                        ev.key = SDLK_UNDERSCORE; break;
                    case SDLK_EQUALS:
                        ev.key = SDLK_PLUS; break;
                    case SDLK_COMMA:
                        ev.key = SDLK_LESS; break;
                    case SDLK_PERIOD:
                        ev.key = SDLK_GREATER; break;
                    case SDLK_SLASH:
                        ev.key = SDLK_QUESTION; break;
                    case SDLK_SEMICOLON:
                        ev.key = SDLK_COLON; break;
                    case SDLK_QUOTE:
                        ev.key = SDLK_QUOTEDBL; break;
                    default:
                        break;
                    }
                }
            }
            break;
        }
        break;

	case SDL_CONTROLLERBUTTONDOWN:
	case SDL_CONTROLLERBUTTONUP:
		switch (sdlev.cbutton.button)
		{
			//AR convert to key events
		case SDL_CONTROLLER_BUTTON_START:	ev.key = JK_ENTER;	break;//enter
		case SDL_CONTROLLER_BUTTON_GUIDE:	ev.key = JK_F1;		break;//help
		case SDL_CONTROLLER_BUTTON_BACK:	ev.key = JK_ESC;	break;//go back
			//
		case SDL_CONTROLLER_BUTTON_A:	ev.key = get_key_binding(get_ctr_binding("ctr_a").c_str(),0);	break;
		case SDL_CONTROLLER_BUTTON_B:	ev.key = get_key_binding(get_ctr_binding("ctr_b").c_str(),0);	break;
		case SDL_CONTROLLER_BUTTON_X:	ev.key = get_key_binding(get_ctr_binding("ctr_x").c_str(),0);	break;
		case SDL_CONTROLLER_BUTTON_Y:	ev.key = get_key_binding(get_ctr_binding("ctr_y").c_str(),0);	break;
			//
		case SDL_CONTROLLER_BUTTON_LEFTSTICK:		ev.key = get_key_binding(get_ctr_binding("ctr_lst").c_str(),0);	break;
		case SDL_CONTROLLER_BUTTON_RIGHTSTICK:		ev.key = get_key_binding(get_ctr_binding("ctr_rst").c_str(),0);	break;
		case SDL_CONTROLLER_BUTTON_LEFTSHOULDER:	ev.key = get_key_binding(get_ctr_binding("ctr_lsr").c_str(),0);	break;
		case SDL_CONTROLLER_BUTTON_RIGHTSHOULDER:	ev.key = get_key_binding(get_ctr_binding("ctr_rsh").c_str(),0);	break;
			//
		case SDL_CONTROLLER_BUTTON_DPAD_UP:		use_left_stick = false;ev.key = get_key_binding("up",0);	break;
		case SDL_CONTROLLER_BUTTON_DPAD_DOWN:	use_left_stick = false;ev.key = get_key_binding("down",0);	break;
		case SDL_CONTROLLER_BUTTON_DPAD_LEFT:	use_left_stick = false;ev.key = get_key_binding("left",0);	break;
		case SDL_CONTROLLER_BUTTON_DPAD_RIGHT:	use_left_stick = false;ev.key = get_key_binding("right",0);	break;
			//
		default:
			// Still want to process this as a key press if only to allow the
			// controller to skip the intro screen.
			ev.key = -1;
		}
		ev.type = sdlev.type == SDL_CONTROLLERBUTTONDOWN ? EV_KEY : EV_KEYRELEASE;
		break;

	case SDL_CONTROLLERAXISMOTION:
		switch (sdlev.caxis.axis)
		{
		case SDL_CONTROLLER_AXIS_LEFTX:
			if(abs(sdlev.caxis.value) >= settings.ctr_lst_dzx) use_left_stick = true;//enable the left stick			

			if(use_left_stick)
			{
				if (sdlev.caxis.value < 0)
				{
					ev.key = get_key_binding("left", 0);
					//AR we need to turn off both right key states when activating left movement, so it doesn't move to the right
					the_game->set_key_down(get_key_binding("right", 0),0);
					the_game->set_key_down(get_key_binding("right2", 0),0);
				}
				else
				{
					ev.key = get_key_binding("right", 0);
					//AR we need to turn off both left key states when activating right movement, so it doesn't move to the left
					the_game->set_key_down(get_key_binding("left", 0),0);
					the_game->set_key_down(get_key_binding("left2", 0),0);
				}

				if(abs(sdlev.caxis.value) < settings.ctr_lst_dzx)
				{
					ev.type = EV_KEYRELEASE;
					//AR stop everything
					the_game->set_key_down(get_key_binding("left", 0),0);
					the_game->set_key_down(get_key_binding("left2", 0),0);
					the_game->set_key_down(get_key_binding("right", 0),0);
					the_game->set_key_down(get_key_binding("right2", 0),0);
				}
				else ev.type = EV_KEY;
			}
			break;

		case SDL_CONTROLLER_AXIS_LEFTY:
			if(abs(sdlev.caxis.value) >= settings.ctr_lst_dzy) use_left_stick = true;//enable the left stick			

			if(use_left_stick)
			{
				if(sdlev.caxis.value < 0)
				{
					ev.key = get_key_binding("up", 0);
					//AR we need to turn off both right key states when activating left movement, so it doesn't move to the right
					the_game->set_key_down(get_key_binding("down", 0),0);
					the_game->set_key_down(get_key_binding("down2", 0),0);
				}
				else
				{
					ev.key = get_key_binding("down", 0);
					//AR we need to turn off both left key states when activating right movement, so it doesn't move to the left
					the_game->set_key_down(get_key_binding("up", 0),0);
					the_game->set_key_down(get_key_binding("up2", 0),0);
				}

				if(abs(sdlev.caxis.value) < settings.ctr_lst_dzy)
				{
					ev.type = EV_KEYRELEASE;
					//AR stop everything
					the_game->set_key_down(get_key_binding("up", 0),0);
					the_game->set_key_down(get_key_binding("up2", 0),0);
					the_game->set_key_down(get_key_binding("down", 0),0);
					the_game->set_key_down(get_key_binding("down2", 0),0);
				}
				else ev.type = EV_KEY;
			}
			break;

			//AR just save the values and update aim inside the game loop
		case SDL_CONTROLLER_AXIS_RIGHTX:	settings.ctr_aim_x = sdlev.caxis.value;	break;
		case SDL_CONTROLLER_AXIS_RIGHTY:	settings.ctr_aim_y = sdlev.caxis.value;	break;

		case SDL_CONTROLLER_AXIS_TRIGGERLEFT:
			//AR convert to key events
			ev.key = get_key_binding(get_ctr_binding("ctr_ltg").c_str(),0);
			if(sdlev.caxis.value > m_dead_zone) ev.type = EV_KEY;
			else ev.type = EV_KEYRELEASE;
			break;

		case SDL_CONTROLLER_AXIS_TRIGGERRIGHT:
			//AR convert to key events
			ev.key = get_key_binding(get_ctr_binding("ctr_rtg").c_str(),0);
			if(sdlev.caxis.value > m_dead_zone) ev.type = EV_KEY;
			else ev.type = EV_KEYRELEASE;
			break;
		}
	}
}
