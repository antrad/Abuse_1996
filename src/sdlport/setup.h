/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *  Copyright (c) 2016 Antonio Radojkovic <antonior.software@gmail.com>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef _SETUP_H_
#define _SETUP_H_

#include <string>

class Settings
{
public:
	//screen
	int		fullscreen;		//0 - window, 1 - fullscreen window, 2 - "real" fullscreen
	bool	borderless;		//borderless window
	bool	vsync;
	short	xres;			//game screen resolution
	short	yres;	
	short	scale;			//windows scale
	bool	linear_filter;	//"antialias"
	int		hires;			//enable hires screens and icons	

	//sound
	bool	mono;
	bool	no_sound;
	bool	no_music;	
	int		volume_sound;	//0-127
	int		volume_music;	//0-127

	//random
	bool	local_save;
	bool	grab_input;			//lock the input to the window
	bool	editor;				//enable editor mode	
	short	physics_update;		//custom pysics update time in miliseconds
	short	mouse_scale;		//mouse scaling in fullscreen, 0 - match desktop, 1 - match game screen
	bool	big_font;			//big font doesn't render properly (there are lines under letters and stuff)
	//
	short	overlay; //AR ???
	bool	in_game;
	bool	bullet_time;		//every game should have it
	float	bullet_time_add;	//percentage

	std::string quick_load;			//quick load
	bool player_touching_console;	//only allow quicksave if player is touching the console

	//cheats
	bool cheat_god, cheat_bullettime;

	//player controls
	int		left, right, up, down;
	int		left_2, right_2, up_2, down_2;
    int		b1;	//special
	int		b2;	//fire
	int		b3;	//weapon prev
	int		b4;	//weapon next
	int		bt;	//bullet time

	//controller settings
	bool	ctr_aim;								//enable
	int		ctr_aim_correctx;						//for some reason game adds black bars on widescreen resolutions and it messes up crosshair position
	int		ctr_cd;									//crosshair distance from player
	int		ctr_rst_s;								//right stick sensitivity
	int		ctr_rst_dz, ctr_lst_dzx, ctr_lst_dzy;	//dead zones
	//
	float	ctr_aim_x, ctr_aim_y;					//state of right stick
	float	ctr_mouse_x, ctr_mouse_y;				//use left stick to move mouse...gave up
	
	//controller buttons
	std::string ctr_a, ctr_b, ctr_x, ctr_y;
	std::string ctr_lst, ctr_rst;					//stick buttons
	std::string ctr_lsr, ctr_rsr;					//shoulder buttons
	std::string ctr_ltg, ctr_rtg;					//trigger buttons

	int ctr_f5, ctr_f9;

	Settings();

	bool CreateConfigFile	(std::string file_path);
	
	bool ControllerButton	(std::string c, std::string b);
	bool ReadConfigFile		(std::string folder);

};

#endif // _SETUP_H_
