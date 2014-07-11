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

#include "SDL.h"

#ifdef HAVE_OPENGL
#   ifdef __APPLE__
#       include <OpenGL/gl.h>
#       include <OpenGL/glu.h>
#   else
#       ifdef WIN32
#           include <Windows.h>
#       endif
#       include <GL/gl.h>
#       include <GL/glu.h>
#   endif    /* __APPLE__ */
#endif    /* HAVE_OPENGL */

#include "common.h"

#include "filter.h"
#include "video.h"
#include "image.h"
#include "setup.h"
#include "errorui.h"

SDL_Window *window = NULL;
SDL_Renderer *renderer = NULL;
SDL_Surface *surface = NULL;
SDL_Surface *screen = NULL;
SDL_Texture *texture = NULL;
image *main_screen = NULL;
int mouse_xscale, mouse_yscale;
int xres, yres;

extern palette *lastl;
extern flags_struct flags;

static void update_window_part(SDL_Rect *rect);

//
// power_of_two()
// Get the nearest power of two
//
static int power_of_two(int input)
{
    int value;
    for(value = 1 ; value < input ; value <<= 1);
    return value;
}

//
// set_mode()
// Set the video mode
//
void set_mode(int mode, int argc, char **argv)
{

    // Set the icon for this window.  Looks nice on taskbars etc.
    //SDL_WM_SetIcon(SDL_LoadBMP("abuse.bmp"), NULL);

    window = SDL_CreateWindow("Abuse",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        0, 0,
        SDL_WINDOW_FULLSCREEN_DESKTOP);
    if(window == NULL)
    {
        show_startup_error("Video : Unable to create window : %s", SDL_GetError());
        exit(1);
    }
    renderer = SDL_CreateRenderer(window, -1, 0);
    if (renderer == NULL)
    {
        show_startup_error("Video : Unable to create renderer : %s", SDL_GetError());
        exit(1);
    }
    SDL_RenderSetLogicalSize(renderer, xres, yres);

    // Create the screen image
    main_screen = new image(ivec2(xres, yres), NULL, 2);
    if(main_screen == NULL)
    {
        // Our screen image is no good, we have to bail.
        show_startup_error("Video : Unable to create screen image.");
        exit(1);
    }
    main_screen->clear();


/*#ifdef HAVE_OPENGL
        int w, h;

        // texture width/height should be power of 2
        // FIXME: we can use GL_ARB_texture_non_power_of_two or
        // GL_ARB_texture_rectangle to avoid the extra memory allocation
        w = power_of_two(xres);
        h = power_of_two(yres);

        // create texture surface
        texture = SDL_CreateRGBSurface(SDL_SWSURFACE, w , h , 32,
#if SDL_BYTEORDER == SDL_LIL_ENDIAN
                0x000000FF, 0x0000FF00, 0x00FF0000, 0xFF000000);
#else
                0xFF000000, 0x00FF0000, 0x0000FF00, 0x000000FF);
#endif

        // setup 2D gl environment
        glPushAttrib(GL_ENABLE_BIT);
        glDisable(GL_DEPTH_TEST);
        glDisable(GL_CULL_FACE);
        glEnable(GL_TEXTURE_2D);

        glViewport(0, 0, window->w, window->h);

        glMatrixMode(GL_PROJECTION);
        glPushMatrix();
        glLoadIdentity();

        glOrtho(0.0, (GLdouble)window->w, (GLdouble)window->h, 0.0, 0.0, 1.0);

        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        glLoadIdentity();

        // texture coordinates
        texcoord[0] = 0.0f;
        texcoord[1] = 0.0f;
        texcoord[2] = (GLfloat)xres / texture->w;
        texcoord[3] = (GLfloat)yres / texture->h;

        // create an RGBA texture for the texture surface
        glGenTextures(1, &texid);
        glBindTexture(GL_TEXTURE_2D, texid);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, flags.antialias);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, flags.antialias);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, texture->w, texture->h, 0, GL_RGBA, GL_UNSIGNED_BYTE, texture->pixels);
#endif
    }*/
    SDL_DisplayMode windowMode;
    SDL_GetWindowDisplayMode(window, &windowMode);
    // Calculate the window scale
    printf("Mouse scaling: [%dx%d] => [%dx%d]\n", windowMode.w, windowMode.h, xres, yres);
    mouse_xscale = (windowMode.w << 16) / xres;
    mouse_yscale = (windowMode.h << 16) / yres;
    printf("Mouse=[%xx%x]\n", mouse_xscale, mouse_yscale);

    // Create our 8-bit surface
    surface = SDL_CreateRGBSurface(0, xres, yres, 8, 0, 0, 0, 0);
    if(surface == NULL)
    {
        // Our surface is no good, we have to bail.
        show_startup_error("Video : Unable to create 8-bit surface: %s", SDL_GetError());
        exit(1);
    }
    // Create our surface for the OpenGL texture
    screen = SDL_CreateRGBSurface(0, xres, yres, 32, 0, 0, 0, 0);
    if (screen == NULL)
    {
        show_startup_error("Video : Unable to create 32-bit surface: %s", SDL_GetError());
        exit(1);
    }
    // And create our OpenGL texture
    texture = SDL_CreateTexture(renderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_STREAMING,
        xres, yres);
    if (texture == NULL)
    {
        show_startup_error("Video : Unable to create texture: %s", SDL_GetError());
        exit(1);
    }

    printf("Video : %dx%d %dbpp\n", windowMode.w, windowMode.h, SDL_BITSPERPIXEL(windowMode.format));

    // Grab and hide the mouse cursor
    SDL_ShowCursor(0);
    // I think grabbing was removed in SDL2
    //if(flags.grabmouse)
    //    SDL_WM_GrabInput(SDL_GRAB_ON);

    update_dirty(main_screen);
}

//
// close_graphics()
// Shutdown the video mode
//
void close_graphics()
{
    if(lastl)
        delete lastl;
    lastl = NULL;
    // Free our 8-bit surface
    if(surface)
        SDL_FreeSurface(surface);
    if (screen)
        SDL_FreeSurface(screen);
    if (texture)
        SDL_DestroyTexture(texture);
    delete main_screen;
}

// put_part_image()
// Draw only dirty parts of the image
//
void put_part_image(image *im, int x, int y, int x1, int y1, int x2, int y2)
{
    int xe, ye;
    SDL_Rect srcrect, dstrect;
    int ii, jj;
    int srcx, srcy, xstep, ystep;
    Uint8 *dpixel;
    Uint16 dinset;

    if(y > yres || x > xres)
        return;

    CHECK(x1 >= 0 && x2 >= x1 && y1 >= 0 && y2 >= y1);

    // Adjust if we are trying to draw off the screen
    if(x < 0)
    {
        x1 += -x;
        x = 0;
    }
    srcrect.x = x1;
    if(x + (x2 - x1) >= xres)
        xe = xres - x + x1 - 1;
    else
        xe = x2;

    if(y < 0)
    {
        y1 += -y;
        y = 0;
    }
    srcrect.y = y1;
    if(y + (y2 - y1) >= yres)
        ye = yres - y + y1 - 1;
    else
        ye = y2;

    if(srcrect.x >= xe || srcrect.y >= ye)
        return;

    // Scale the image onto the surface
    srcrect.w = xe - srcrect.x;
    srcrect.h = ye - srcrect.y;
    dstrect.x = x;
    dstrect.y = y;
    dstrect.w = srcrect.w;
    dstrect.h = srcrect.h;

    xstep = (srcrect.w << 16) / dstrect.w;
    ystep = (srcrect.h << 16) / dstrect.h;

    srcy = ((srcrect.y) << 16);
    dinset = ((surface->w - dstrect.w)) * surface->format->BytesPerPixel;

    // Lock the surface if necessary
    if(SDL_MUSTLOCK(surface))
        SDL_LockSurface(surface);

    dpixel = (Uint8 *)surface->pixels;
    dpixel += (dstrect.x + ((dstrect.y) * surface->w)) * surface->format->BytesPerPixel;

    // Update surface part
    srcy = srcrect.y;
    dpixel = ((Uint8 *)surface->pixels) + y * surface->w + x ;
    for(ii=0 ; ii < srcrect.h; ii++)
    {
        memcpy(dpixel, im->scan_line(srcy) + srcrect.x , srcrect.w);
        dpixel += surface->w;
        srcy ++;
    }

    // Unlock the surface if we locked it.
    if(SDL_MUSTLOCK(surface))
        SDL_UnlockSurface(surface);

    // Now blit the surface
    update_window_part(&dstrect);
}

//
// load()
// Set the palette
//
void palette::load()
{
    if(lastl)
        delete lastl;
    lastl = copy();

    // Force to only 256 colours.
    // Shouldn't be needed, but best to be safe.
    if(ncolors > 256)
        ncolors = 256;

#ifdef WIN32
	// FIXME: Really, this applies to anything that doesn't allow dynamic stack allocation
	SDL_Color colors[256];
#else
    SDL_Color colors[ncolors];
#endif
    for(int ii = 0; ii < ncolors; ii++)
    {
        colors[ii].r = red(ii);
        colors[ii].g = green(ii);
        colors[ii].b = blue(ii);
        colors[ii].a = 255;
    }
    SDL_SetPaletteColors(surface->format->palette, colors, 0, ncolors);

    // Now redraw the surface
    update_window_part(NULL);
    update_window_done();
}

//
// load_nice()
//
void palette::load_nice()
{
    load();
}

// ---- support functions ----

void update_window_done()
{
    // Convert to match the OpenGL texture
    SDL_BlitSurface(surface, NULL, screen, NULL);
    // Copy over to the OpenGL texture
    SDL_UpdateTexture(texture, NULL, screen->pixels, screen->pitch);
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, texture, NULL, NULL);
    SDL_RenderPresent(renderer);
}

static void update_window_part(SDL_Rect *rect)
{
    // This should now be a complete no-op
#if 0
    // no partial blit's in case of opengl
    // complete blit + scaling just before flip
    if (flags.gl)
        return;

    SDL_BlitSurface(surface, rect, window, rect);

    // no window update needed until end of run
    if(flags.doublebuf)
        return;

    // update window part for single buffer
    if(rect == NULL)
        SDL_UpdateRect(window, 0, 0, 0, 0);
    else
        SDL_UpdateRect(window, rect->x, rect->y, rect->w, rect->h);
#endif
}
