/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 2001 Anthony Kruize <trandor@labyrinth.net.au>
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
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

#include <stdio.h>

#include "SDL.h"
#include "joy.h"

/* NOTE: No joystick support yet.
 */

int joy_init( int argc, char **argv )
{
    int joysticks = SDL_NumJoysticks();
    printf("%d joysticks on system\n", joysticks);
    for (int i = 0; i < joysticks; i++) {
        if (SDL_IsGameController(i))
        {
            if (SDL_GameControllerOpen(i) == NULL)
            {
                const char* error = SDL_GetError();
                printf("Warning : Unable to open game controller %s: %s\n", SDL_JoystickNameForIndex(i), error);
            }
        }
        printf("  - joystick %d (%s) : %s\n", i, SDL_IsGameController(i) ? "controller" : " joystick ", SDL_JoystickNameForIndex(i));
    }
    return joysticks > 0;
}

void joy_status( int &b1, int &b2, int &b3, int &xv, int &yv )
{
    /* Do Nothing */
}

void joy_calibrate()
{
    /* Do Nothing */
}
