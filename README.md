Abuse README
============

This is a fork of the Abuse SDL2 port from <http://github.com/Xenoveritas/abuse>,
which itself is a fork of the original Abuse SDL port from <http://abuse.zoy.org>.

----

1. Introduction
2. Additional Features
3. Building The Project
4. Configuration
5. Hardcoded keys
6. Notes
7. Special Thanks
8. Feedback
9. Links

----

## 1. INTRODUCTION

When I released my Quake 2D demo back in 2012 many people compared it to Abuse. While I was waiting for my new PC to get fixed I was stuck with a PC bought in 2005.
My gaming options were limited, and I found out Abuse was available for free, so I wanted to check it out:
- It ran in DOSBox at 320x200 resolution, which is fine, but since it was a shooter it felt disorienting, like playing a FPS in a very low FOV.
- The screen tearing was horrible and I couldn't enable vsync.
- Aiming with the mouse was very difficult, because, even in fullscreen, the mouse was still behaving like it was 320x200 resolution and was too sensitive.

Reading the readme file I saw there was a high resolution option, but it seemed to be only available in the shareware version, or in editor mode,
and the game would automatically turn off the in-game lights, because it would be too demanding for the PCs in 1996 on high resolutions.
Not to mention a bug would cause the entire screen to go black the second time a level was loaded.

Then I found out the source code was released and was looking for modern ports:
- The 2001 SDL port was for Linux only.
- Then I found a 2001 Windows port, but the game would not run. I managed to get the sorce code to compile and fixed the crashing issue,
  but I only got around 10-15 frames per minute.
- Then I found the 2011 version, and again it was Linux only.
- Then finally the Xenoveritas port showed up in the search results, and was exactly what I needed; a working Windows port.

## 2. ADDITIONAL FEATURES

These are the major changes I made compared to Xenoveritas version:

  * Enabled custom resolutions and enabled lights at high resolutions
  * Re-enabled OpenGL rendering to enable vsync
  * Game screen scaling in window or fullscreen mode using F11 and F12
  * Added or re-enabled several settings in the config file
  * Physics update time can be changed via config file
  * Local save game files and configuration files
  * XBox360 controller support with rebindable buttons via the config file (toggle controller aiming using F9)
  * Updated abuse-tool so it can extract the images in Abuse SPEC files to modern image formats
	as individual images, tilemaps or a texture atlas with information about image, tile and animation frame sizes and positions.
	
	***!!! moga bi link stavit ovdje !!!***

## 3. BUILDING THE PROJECT

Abuse has the following requirements:

  * SDL2 2.0.3 or above.
  * SDL_mixer 2.0.0 or above.
  * GLee for OpenGL rendering
  * OpenCV 2.1 for abuse-tool

Read the BUILDING.md file provided by Xenoveritas to see how to build the projects.
Do note this version also uses OpenGL(GLee) and OpenCV, so you will need to link to those libraries too.

## 4. CONFIGURATION

Abuse configuration file has been updated in this version. The file is stored locally in the "user" folder as "config.txt",
where also the save game files and other original configuration files, like gamma settings, can be found.

Lines starting with a ';' are comments. Setting an option to '1' turns it on, and '0' turns it off.
Following settings can be changed via the config file:

fullscreen - fullscreen or window mode
vsync - vertical sync
screen_width - game screen width
screen_height - game screen height
scale - window scale
linear_filter - use linear texture filter (nearest is default)

volume_sound - sound volume
volume_music - music volume
mono - use mono audio only
no_music - disable music
no_sound - disable sound effects

local_save - save config and other files locally
grab_mouse - grab the mouse to the window
editor - enable editor mode
physics_update - physics update time in ms
mouse_scale - mouse to game scaling based on desktop or game screen size

To change the keys used in the game, simply type the key after the option:
left - move left
right - move right
up - climb ladder
down - use lift, press switch
special - use special ability
fire - fire weapon
weapon_prev - select previous available weapon
weapon_next - select next available weapon

The following special keys can also be used:

| Code                          | Represents
|-------------------------------|-----------------------
| `LEFT`, `RIGHT`, `UP`, `DOWN` | Cursor keys and keypad.
| `CTRL_L`, `CTRL_R`            | Left and right Ctrl keys.
| `ALT_L`, `ALT_R`              | Left and right Alt keys.
| `SHIFT_L`, `SHIFT_R`          | Left and right Shift keys.
| `F1` - `F10`                  | Function keys 1 through 10.
| `TAB`                         | Tab key.
| `BACKSPACE`                   | Backspace key.
| `ENTER`                       | Enter key
| `INSERT`, `DEL`               | Insert and Delete keys.
| `PAGEUP`, `PAGEDOWN`          | Page Up and Page Down keys.
| `CAPS`, `NUM_LOCK`            | Caps-Lock and Num-Lock keys.
| `SPACE`                       | Spacebar.

The default key settings are as follows:

| Action      | Bound to
|-------------|---------
| Left        | Left arrow, A
| Right       | Right arrow, D
| Up/Jump     | Up arrow, W
| Down/Use    | Down arrow, S
| Prev Weapon | Left or Right Ctrl
| Next Weapon | Insert

The mouse controls your aim, with Left button for fire and Right button for special.
The mouse wheel can be used for changing weapons.

The game has almost full controller support now, there are several settings you can change in the config file:
ctr_aim - enable aiming with the right stick
ctr_cd - crosshair distance from player
ctr_rst_s - right stick/aiming sensitivity
ctr_rst_dz - right stick/aiming dead zone
ctr_lst_dzx - left stick left/right movement dead zones
ctr_lst_dzy - left stick up/down movement dead zones

To bind controller buttons to in game action use the following names for the buttons;
ctr_a, ctr_b, ctr_x, ctr_y
ctr_left_shoulder, ctr_right_shoulder
ctr_left_trigger, ctr_right_trigger
ctr_left_stick, ctr_right_stick

See "5. Hardcoded keys" for the hardcoded controller bindings.

## 5. Hardcoded keys

There are several keys in the game that are hardcoded to some function originally or were added during porting:

- 1-7 - weapon selection
- Home, control left, control right - previous weapon
- Page up, insert - next weapon
- Numpad 2,4,5,6,8 - player movement 
- escape, space, enter - reset level on death
- h, F1 - show help/controls screen
- c - chat console
- p - pause game
- F8 - toggle mouse scaling
- F9 - toggle controller aiming
- F10 - toggle window/fullscreen mode
- F11 - scale window/screen up
- F12 - scale window/screen down
- Print screen - take a screenshot

Controller defaults:

- D-pad, left stick - move left, right, up, down in game and in menus
- home - show help/controls screen
- back - behaves like the escape key
- start - behaves like enter key

## 6. NOTES

### Low rendering speed:
	If I understand the rendering process correctly, everything is first rendered to a in game image format by copying bytes of data from the image buffers.
	Then those are again copied to SDL_Surface, then SDL_BlitSurface is called to convert the pixel values to OpenGL format. The resulting SDL_Surface pixel buffer
	is then used to set the pixels of the OpenGL texture using glTexImage2D. Just then we finally render to the backbuffer and update the screen.
	There must be a way to skip some of these steps, you can check out "sdlport/video.cpp" if you have an idea how.

### 15 FPS:
	The game is designed to run at 15 FPS, higher or lower framerate speeds up or slows down the game. I haven't figured out how physics, animations and
	other time based stuff work, to make the game world update normally at higer framerate. While the game is rendered at higher framerate,
	I have set up a timer that limits the game physics update speed with a default value of 65 ms(15 FPS).
	
### Music volume lowers the sound effects volume too:
	May have something to do with it being MIDI music, I don't know.

## 7. SPECIAL THANKS

To everybody who worked on Abuse and its ports; from the people at Crack Dot Com who made the original game,
to Xenoveritas and others who kept it alive for 20 years.

## 8. FEEDBACK

I am more or less done with this project. I might update the abuse-tool to extract the levels to a useable format.
If you find bugs or have some problems with the game send me an email and I will see what I can do.

## 9. LINKS

Original source code https://archive.org/details/abuse_sourcecode
Jeremy Scott's Windows port [2001] http://web.archive.org/web/20051023123223/http://www.webpages.uidaho.edu/~scot4875/
Abuse SDL [2002] http://web.archive.org/web/20070205093016/http://www.labyrinth.net.au/~trandor/abuse/
Sam Hocevar Abuse Page [2011] http://abuse.zoy.org/
Xenoveritas SDL2 port [2014] http://github.com/Xenoveritas/abuse

Abuse home page http://web.archive.org/web/20010517011228/http://abuse2.com/
Free Abuse (Frabs) home page http://web.archive.org/web/20010124070000/http://www.cs.uidaho.edu/~cass0664/fRaBs/

Frabs download http://www.dosgames.com/g_act.php
Abuse Desura download http://www.desura.com/games/abuse/download

HMI to MIDI converter http://www.ttdpatch.net/midi/games.html

----

Thank you for playing Abuse!