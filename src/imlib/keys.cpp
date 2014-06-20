/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#if defined HAVE_CONFIG_H
#   include "config.h"
#endif

#include <string.h>
#include <ctype.h>

#include "keys.h"

char const *jk_key_names[]=
{
    "Up Arrow","Down Arrow","Left Arrow","Right Arrow",
    "Left Ctrl","Right Ctrl","Left Alt","Right Alt",
    "Left Shift","Right Shift","Caps Lock","Num Lock",
    "Home","End","Del","F1","F2","F3","F4","F5","F6",
    "F7","F8","F9","F10","Insert","PageUp","PageDown","Command"
};

void key_name(int key, char *buffer)
{
    //static char sing[2];
    if( key > 255 && key <= JK_MAX_KEY )
        strcpy(buffer,jk_key_names[key-256]);
    else if( key == JK_BACKSPACE )
        strcpy(buffer,"Backspace");
    else if( key == JK_TAB )
        strcpy(buffer,"Tab");
    else if( key == JK_ENTER )
        strcpy(buffer,"Enter");
    else if( key == JK_ESC )
        strcpy(buffer,"Esc");
    else if( key == JK_SPACE )
        strcpy( buffer, "Space" );
    else if( isprint(key) )
    {
        buffer[0] = key;
        buffer[1] = 0;
    }
    else
    {
        buffer[0] = 0;
    }
}

int key_value(char const *buffer)
{
    if( strcasecmp( buffer, "Backspace" ) == 0 )
        return JK_BACKSPACE;
    if( strcasecmp( buffer, "Tab" ) == 0 )
        return JK_TAB;
    if( strcasecmp( buffer, "Enter" ) == 0 )
        return JK_ENTER;
    if( strcasecmp( buffer, "ESC" ) == 0 )
        return JK_ESC;
    if( strcasecmp( buffer, "Space" ) == 0 )
        return JK_SPACE;
    if( strcasecmp( buffer, "Up" ) == 0 )
        return JK_UP;
    if( strcasecmp( buffer, "Down" ) == 0 )
        return JK_DOWN;
    if( strcasecmp( buffer, "Left" ) == 0 )
        return JK_LEFT;
    if( strcasecmp( buffer, "Right" ) == 0 )
        return JK_RIGHT;
    if( strcasecmp( buffer, "CTRL_L" ) == 0 )
        return JK_CTRL_L;
    if( strcasecmp( buffer, "CTRL_R" ) == 0 )
        return JK_CTRL_R;
    if( strcasecmp( buffer, "ALT_L" ) == 0 )
        return JK_ALT_L;
    if( strcasecmp( buffer, "ALT_R" ) == 0 )
        return JK_ALT_R;
    if( strcasecmp( buffer, "SHIFT_L" ) == 0 )
        return JK_SHIFT_L;
    if( strcasecmp( buffer, "SHIFT_R" ) == 0 )
        return JK_SHIFT_R;
    if( strcasecmp( buffer, "Caps" ) == 0 )
        return JK_CAPS;
    if( strcasecmp( buffer, "Num_Lock" ) == 0 )
        return JK_NUM_LOCK;
    if( strcasecmp( buffer, "Home" ) == 0 )
        return JK_HOME;
    if( strcasecmp( buffer, "End" ) == 0 )
        return JK_END;
    if( strcasecmp( buffer, "Del" ) == 0 )
        return JK_DEL;
    if( strcasecmp( buffer, "F1" ) == 0 )
        return JK_F1;
    if( strcasecmp( buffer, "F2" ) == 0 )
        return JK_F2;
    if( strcasecmp( buffer, "F3" ) == 0 )
        return JK_F3;
    if( strcasecmp( buffer, "F4" ) == 0 )
        return JK_F4;
    if( strcasecmp( buffer, "F5" ) == 0 )
        return JK_F5;
    if( strcasecmp( buffer, "F6" ) == 0 )
        return JK_F6;
    if( strcasecmp( buffer, "F7" ) == 0 )
        return JK_F7;
    if( strcasecmp( buffer, "F8" ) == 0 )
        return JK_F8;
    if( strcasecmp( buffer, "F9" ) == 0 )
        return JK_F9;
    if( strcasecmp( buffer, "F10" ) == 0 )
        return JK_F10;
    if( strcasecmp( buffer, "Insert" ) == 0 )
        return JK_INSERT;
    if( strcasecmp( buffer, "PageUp" ) == 0 )
        return JK_PAGEUP;
    if( strcasecmp( buffer, "PageDown" ) == 0 )
        return JK_PAGEDOWN;

    // Assume they are asking for a normal key, so convert the first
    // character to an integer and return that
    return (int)buffer[0];
}

