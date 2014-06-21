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

#ifdef WIN32
# include <Windows.h>
#else
# include <stdio.h>
# include <stdlib.h>
# if defined HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
# include <time.h>
#endif

#include "timing.h"

#ifdef __APPLE__
// OSX 10.1 has nanosleep but no header for it!
extern "C" {
int nanosleep(const struct timespec *rqtp, struct timespec *rmtp);
}
#endif

// Constructor
//
time_marker::time_marker()
{
    get_time();
}

//
// get_time()
// Get the current time
//
void time_marker::get_time()
{
#ifdef WIN32
	// Use GetSystemTimeAsFileTime for this
	GetSystemTimeAsFileTime(&ticks);
#else
    struct timeval tv = { 0, 0 };
    gettimeofday( &tv, NULL );
    seconds = tv.tv_sec;
    micro_seconds = tv.tv_usec;
#endif
}

//
// diff_time()
// Find the time difference
//
double time_marker::diff_time( time_marker *other )
{
#if defined WIN32
	// Convert both sides to __int64
	__int64 our_ticks = (ticks.dwHighDateTime << 32L) | ticks.dwLowDateTime;
	__int64 other_ticks = (other->ticks.dwHighDateTime << 32L) | (other->ticks.dwLowDateTime);
	// Note we're dividing by 10,000,000 here - ticks are in 100ns increments, not microseconds.
	return (double)(our_ticks - other_ticks) / 10000000.0;
#else
    return (double)(seconds - other->seconds) + (double)(micro_seconds - other->micro_seconds) / 1000000;
#endif
}

