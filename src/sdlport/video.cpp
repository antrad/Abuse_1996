/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 2001 Anthony Kruize <trandor@labyrinth.net.au>
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *	Copyright (c) 2016 Antonio Radojkovic <antonior.software@gmail.com>
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

//AR OpenGL - Windows might complain if we include OpenGL before windows.h
#ifdef WIN32
#include <windows.h>
#endif
#include "Glee.h"
//Abuse linker->input:
//	- opengl32.lib
//	- GLee.lib
//

#include "SDL.h"

#include "common.h"

#include "filter.h"
#include "video.h"
#include "image.h"
#include "setup.h"
#include "errorui.h"

//AR all rendering goes to "main_screen" all over the code
//then using put_part_image() it is rendered to "surface"...I think
//then it is rendered to screen using update_window_done() where the surface is first copied to a OGL texture

SDL_Window *window = NULL;
SDL_Surface *surface = NULL;
SDL_Surface *screen = NULL;
image *main_screen = NULL;

int mouse_xpad, mouse_ypad, mouse_xscale, mouse_yscale;
int xres, yres;

extern palette *lastl;
extern Settings settings;

void calculate_mouse_scaling();

//AR OpenGL
SDL_GLContext glcontext;
GLuint ar_texture;
SDL_DisplayMode desktop;
int window_w = 320, window_h = 200;
bool ar_fullscreen = false;
int ogl_scale = 1;
int ogl_w = 320, ogl_h = 200;
//

void video_change_settings(int scale_add, bool toggle_fullscreen);

//
// set_mode()
// Set the video mode
//
void set_mode(int argc, char **argv)
{
	int displayIndex = 0;
	desktop.w = 320;
	desktop.h = 200;

	if(SDL_GetDesktopDisplayMode(0,&desktop)!=0) printf("ERROR - failed to get display info\n");

	//AR scale window
    window_w = xres*scale;
	window_h = yres*scale;

	//fullscreen "scale"
	ogl_w = window_w;
	ogl_h = window_h;

	int window_type = 0;

	if(settings.fullscreen==1)		window_type = SDL_WINDOW_FULLSCREEN_DESKTOP;
	else if(settings.fullscreen==2)	window_type = SDL_WINDOW_FULLSCREEN;
    
	// FIXME: Set the icon for this window.  Looks nice on taskbars etc.
    //SDL_WM_SetIcon(SDL_LoadBMP("abuse.bmp"), NULL);

    window = SDL_CreateWindow("Abuse",
        SDL_WINDOWPOS_UNDEFINED,SDL_WINDOWPOS_UNDEFINED,
        window_w, window_h,
        window_type|SDL_WINDOW_OPENGL);

    if(!window)
    {
        show_startup_error("Video : Unable to create window : %s", SDL_GetError());
        exit(1);
    }

	//AR OpenGL
	glcontext = SDL_GL_CreateContext(window);

	if(settings.vsync) SDL_GL_SetSwapInterval(1);

	glPushAttrib(GL_ENABLE_BIT);
	glDisable(GL_DEPTH_TEST);
	glDisable(GL_CULL_FACE);

	glViewport(0,0,window_w,window_h);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0,window_w,window_h,0,-1,1);

	glEnable(GL_TEXTURE_2D);
	glGenTextures(1,&ar_texture);
	glBindTexture(GL_TEXTURE_2D,ar_texture);
	if(settings.linear_filter==1)
	{
		glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
		glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
	}
	else
	{
		glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
		glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
	}
	glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
	glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);

	//Create our surface for the OpenGL texture
	screen = SDL_CreateRGBSurface(SDL_SWSURFACE, xres , yres , 32,
#if SDL_BYTEORDER == SDL_LIL_ENDIAN
                0x000000FF, 0x0000FF00, 0x00FF0000, 0xFF000000);
#else
                0xFF000000, 0x00FF0000, 0x0000FF00, 0x000000FF);
#endif
    if(screen == NULL)
    {
        show_startup_error("Video : Unable to create 32-bit surface: %s", SDL_GetError());
        exit(1);
    }
	//
	
	// Create our 8-bit surface
    surface = SDL_CreateRGBSurface(0, xres, yres, 8, 0, 0, 0, 0);
    if(surface == NULL)
    {
        // Our surface is no good, we have to bail.
        show_startup_error("Video : Unable to create 8-bit surface: %s", SDL_GetError());
        exit(1);
    }

	// Create the screen image
    main_screen = new image(ivec2(xres, yres), NULL, 2);
    if(main_screen == NULL)
    {
        // Our screen image is no good, we have to bail.
        show_startup_error("Video : Unable to create screen image.");
        exit(1);
    }
    main_screen->clear();
	
	//hide the mouse cursor and set up the mouse
    SDL_ShowCursor(0);
    calculate_mouse_scaling(); 

	if(settings.fullscreen!=0) video_change_settings(0,true);

	//print some info
	//AR shows 640x480 when the size is lower than that... ???
	/*SDL_DisplayMode mode;
    SDL_GetWindowDisplayMode(window, &mode);
    printf("Video : %dx%d %dbpp\n", mode.w, mode.h, SDL_BITSPERPIXEL(mode.format));*/

    update_dirty(main_screen);
}

void video_change_settings(int scale_add, bool toggle_fullscreen)
{
	//AR
	if(toggle_fullscreen)
	{
		ar_fullscreen = !ar_fullscreen;
		if(ar_fullscreen)
		{
			SDL_SetWindowFullscreen(window,SDL_WINDOW_FULLSCREEN_DESKTOP);
			SDL_GetWindowSize(window,&window_w,&window_h);
			
			//OGL texture rendering size, scale while if fits
			int scl = 1;
			while(1!=0)
				if(xres*scl<desktop.w && yres*scl<desktop.h)
				{
					ogl_scale = scl;
					ogl_w = xres*ogl_scale;
					ogl_h = yres*ogl_scale;					
					scl++;
				}
				else break;
		}
		else SDL_SetWindowFullscreen(window,0);
	}

	static int overscale = 0;
	
	if(!ar_fullscreen)
	{
		//scale window
		int new_scale = scale + scale_add;

		if(new_scale>0 && xres*new_scale<=desktop.w && yres*new_scale<=desktop.h)
		{
			//scale windows if it fits on screen
			scale = new_scale;
			SDL_SetWindowSize(window,xres*scale,yres*scale);
		}
	}
	else
	{
		//scale OGL texture rendering size
		int new_scale = ogl_scale + scale_add;

		if(overscale==2 && scale_add==-1) overscale = 0;

		if(new_scale>0)
		{
			if(xres*new_scale>=desktop.w || yres*new_scale>=desktop.h)
			{
				if(overscale==0)
				{
					if(yres*((float)desktop.w/xres)<desktop.h)
					{
						//limit scale by monitor width
						ogl_w = desktop.w;
						ogl_h = yres*((float)desktop.w/xres);
					}
					else
					{
						//limit scale by monitor height
						ogl_w = xres*((float)desktop.h/yres);
						ogl_h = desktop.h;
					}
					overscale = 1;
					ogl_scale = new_scale;
				}
				else if(overscale==1)
				{
					//match screen to desktop/monitor size
					ogl_w = desktop.w;
					ogl_h = desktop.h;

					overscale = 2;
					ogl_scale = new_scale;
				}				
			}
			else if(xres*new_scale<=desktop.w && yres*new_scale<=desktop.h)
			{
				//scale and keep aspect
				ogl_scale = new_scale;
				ogl_w = xres*ogl_scale;
				ogl_h = yres*ogl_scale;
				overscale = 0;
			}
		}
	}

	//update size, position and OGL
	SDL_GetWindowSize(window,&window_w,&window_h);
	SDL_SetWindowPosition(window,desktop.w/2-window_w/2,desktop.h/2-window_h/2);

	glViewport(0,0,window_w,window_h);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0,window_w,window_h,0,-1,1);
	//

    calculate_mouse_scaling();
}

void calculate_mouse_scaling()
{
	if(!ar_fullscreen || settings.mouse_scale==0)
	{
		// We need to determine the appropriate mouse scaling
		float scale_x = window_w/xres;
		float scale_y = window_h/yres;

		// Re-calculate the mouse scaling
		mouse_xscale = (window_w << 16) / xres;
		mouse_yscale = (window_h << 16) / yres;

		// And calculate the padding
		mouse_xpad = scale_x;
		mouse_ypad = scale_y;
	}
	else
	{
		// We need to determine the appropriate mouse scaling
		float scale_x = ogl_w/xres;
		float scale_y = ogl_h/yres;

		// Re-calculate the mouse scaling
		mouse_xscale = (ogl_w << 16) / xres;
		mouse_yscale = (ogl_h << 16) / yres;

		// And calculate the padding
		mouse_xpad = scale_x;
		mouse_ypad = scale_y;
	}
}

//
// close_graphics()
// Shutdown the video mode
//
void close_graphics()
{
    if(lastl) delete lastl;
    lastl = NULL;

    // Free our 8-bit surface    
	if(surface) SDL_FreeSurface(surface);
    if(screen) SDL_FreeSurface(screen);

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
	//AR convert to match the OpenGL texture
	SDL_BlitSurface(surface, NULL, screen, NULL);

	//clear the backbuffer
	glClearColor(0,0,0,0);
	glClear(GL_COLOR_BUFFER_BIT);

	//map the surface to the texture in video memory
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, screen->w, screen->h, 0, GL_RGBA, GL_UNSIGNED_BYTE, screen->pixels);
	
	glBegin(GL_POLYGON);
	if(ar_fullscreen)
	{
		//expand from center so we can scale
		glTexCoord2f(0,0); glVertex2i(desktop.w/2 - ogl_w/2, desktop.h/2 - ogl_h/2);
		glTexCoord2f(1,0); glVertex2i(desktop.w/2 + ogl_w/2, desktop.h/2 - ogl_h/2);
		glTexCoord2f(1,1); glVertex2i(desktop.w/2 + ogl_w/2, desktop.h/2 + ogl_h/2);
		glTexCoord2f(0,1); glVertex2i(desktop.w/2 - ogl_w/2, desktop.h/2 + ogl_h/2);
	}
	else
	{
		//match window size
		glTexCoord2f(0,0); glVertex2i(0, 0);
		glTexCoord2f(1,0); glVertex2i(window_w, 0);
		glTexCoord2f(1,1); glVertex2i(window_w, window_h);
		glTexCoord2f(0,1); glVertex2i(0, window_h);	
	}
	glEnd();

	SDL_GL_SwapWindow(window);
	//
}