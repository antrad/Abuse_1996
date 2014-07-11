/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 2014 Daniel Potter
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

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef WIN32
# include <Windows.h>
#endif

#include <stdio.h>
#include <stdarg.h>

#include "errorui.h"

void show_error_message(const char *title, const char* format, ... )
{
    char buffer[1024];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, 1024, format, args);
    va_end(args);
    // Always dump the message to stderr
    fputs(buffer, stderr);
    // And then do something platform-specific with this.
#ifdef WIN32
    MessageBox(NULL, buffer, title == NULL ? "Error" : title, MB_OK | MB_ICONERROR);
#endif
}

void show_startup_error(const char* format, ...)
{
    char buffer[1024];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, 1024, format, args);
    va_end(args);
    show_error_message("Error Starting Abuse", "An error occurred which has prevented Abuse from starting:\n\n%s", buffer);
}
