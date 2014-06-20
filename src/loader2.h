/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __LOADER_HPP_
#define __LOADER_HPP_

#include "lisp/lisp.h"
#include "sdlport/sound.h"

#include "jwindow.h"
class property_manager;
extern property_manager *prop;

/*********************************title screen***********************************/
extern int title_screen;


/*********************************joystick pictures******************************/
extern int joy_picts[2*9];

/*************************** devleopment mode pictures **************************/
extern int light_buttons[13];

/*********************************Fonts******************************************/
extern JCFont *big_font,*console_font;
extern int big_font_pict,small_font_pict,console_font_pict;


/******************************** FIGURES ***************************************/


extern int damage_pict,block_pict;
extern uint16_t current_start_type,start_position_type;


/******************************* SOUND EFFECTS **********************************/
extern int raise_volume,lower_volume,record_button,play_button,music_button,sfx_button;

extern int sfx_volume,music_volume,sound_avail;
extern song *current_song;

/******************************** SCREEN FRAME **********************************/
extern int border_tile,window_texture,
         record_button,play_button,window_colors,pause_image,vmm_image,
         earth,earth_mask,clouds,
         numbers[10], stat_bar,
         ok_button,cancel_button,
         cdc_logo;


/******************************** TILES *****************************************/
extern int *backtiles;                  // array of id's
extern int *foretiles;
extern int nforetiles,nbacktiles,       // total foreground tiles & background tiles
       f_wid,f_hi,b_wid,b_hi;       //  width and height of foreground/background tiles


/******************************** COLOR *****************************************/
extern palette *pal;
extern ColorFilter *color_table;
extern int light_connection_color;


/******************************** MOUSE CURSORS *********************************/
extern int c_mouse1,c_mouse2,c_normal,c_target;

/******************************* GLOBALS ****************************************/
extern long bg_xmul,bg_xdiv,bg_ymul,bg_ydiv;  // brackground scroll rates
extern char mouse_scrolling,palettes_locked,view_shift_disabled;
extern int edit_mode;
extern int start_running;


image *load_image(spec_entry *e, bFILE *fp);      // preforms scaling
image *load_image(bFILE *fp);
void load_data(int argc, char **argv);
char *load_script(char *name);
void load_tiles(Cell *file_list);
extern char lsf[256];

#endif


