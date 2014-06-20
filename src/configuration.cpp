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

#include <ctype.h>

#include "common.h"

#include "sdlport/joy.h"
#include "game.h"

#include "keys.h"
#include "lisp.h"
#include "jwindow.h"
#include "configuration.h"

extern int get_key_binding(char const *dir, int i);

int key_players = 0;
int morph_detail = MEDIUM_DETAIL;

struct player_keys
{
    int joy, left, right, up, down, b1, b2, b3, b4;
};

static player_keys *key_map = NULL;

static int binding_for_player( int player )
{
    char tmp[40];
    sprintf( tmp, "player%d", player );
    LSymbol *f = LSymbol::Find(tmp);
    if( !NILP(f) && DEFINEDP(f->GetValue()))
    {
        void *what = f->GetValue();
        if(what == LSymbol::FindOrCreate("keyboard"))
            return 1;
        else if(what == LSymbol::FindOrCreate("joystick"))
            return 2;
    }
    return 0;
}

/*
int get_key_binding(char const *dir, int i)
{
    char tmp[100], kn[50];
    sprintf( tmp, "player%d-%s", i, dir );
    Cell *f = find_symbol( tmp );
    if( NILP(f) || !DEFINEDP( symbol_value( f ) ) )
        return 0;
    void *k = symbol_value( f );

    if( item_type( k ) != L_SYMBOL )
        return 0;

    strcpy( tmp, lstring_value( symbol_name( k ) ) );

    for( char *c = tmp; *c; c++ )
    {
        *c = tolower( *c );
        if( *c == '_' )
            *c = ' ';
    }

    for( int j = 0; j < JK_MAX_KEY; j++ )
    {
        key_name( j, kn );
        for( char *c = kn; *c; c++ )
        {
            *c = tolower(*c);
        }
        if( !strcmp( kn, tmp ) )
            return j;
    }
    return 0;
}
*/

/*
void get_key_bindings()
{
    if( key_map )
    {
        free( key_map );
    }
    key_map = NULL;

    for( key_players = 0; binding_for_player( key_players + 1); key_players++ );
    if( key_players )
    {
        key_map = ( player_keys *)malloc(sizeof(player_keys)*key_players);
        for( int i = 0; i < key_players; i++ )
        {
            key_map[i].joy = ( binding_for_player( i + 1 ) == 2 );
            if( !key_map[i].joy )
            {
                key_map[i].left = get_key_binding( "left", i + 1 );
                key_map[i].right = get_key_binding( "right", i + 1 );
                key_map[i].up = get_key_binding( "up", i + 1 );
                key_map[i].down = get_key_binding( "down", i + 1 );
                key_map[i].b4 = get_key_binding( "b4", i + 1 );
                key_map[i].b3 = get_key_binding( "b3", i + 1 );
                key_map[i].b2 = get_key_binding( "b2", i + 1 );
                key_map[i].b1 = get_key_binding( "b1", i + 1 );
            }
        }
    }
    else
    {
        key_map = NULL;
    }
}*/

// AK
void get_key_bindings()
{
    if( key_map )
    {
        free( key_map );
    }
    key_map = NULL;

    key_players = 1;
    key_map = (player_keys *)malloc( sizeof( player_keys ) * key_players );
    for( int i = 0; i < key_players; i++ )
    {
        key_map[i].joy = 0;
        key_map[i].left = get_key_binding( "left", i + 1 );
        key_map[i].right = get_key_binding( "right", i + 1 );
        key_map[i].up = get_key_binding( "up", i + 1 );
        key_map[i].down = get_key_binding( "down", i + 1 );
        key_map[i].b4 = get_key_binding( "b4", i + 1 );
        key_map[i].b3 = get_key_binding( "b3", i + 1 );
        key_map[i].b2 = get_key_binding( "b2", i + 1 );
        key_map[i].b1 = get_key_binding( "b1", i + 1 );
    }
}


#define is_pressed(x) the_game->key_down(x)

void get_movement(int player, int &x, int &y, int &b1, int &b2, int &b3, int &b4)
{
    if( player < key_players )
    {
/*        if( key_map[player].joy )
        {
            joy_status( b1,b2,b3,x,y );
            b3 = ( b1 && b2 );
            b4 = 0;
        }
        else if( !wm )
        {
            x = y = b1 = b2 = b3 = b4 = 0;
        }
        else*/
        {
            if( is_pressed( key_map[player].left ) )
                x = -1;
            else if( is_pressed( key_map[player].right ) )
                x=1;
            else
                x = 0;

            if( is_pressed( key_map[player].up ) )
                y = -1;
            else if( is_pressed( key_map[player].down ) )
                y = 1;
            else y = 0;

            if( is_pressed( key_map[player].b1 ) )
                b1 = 1;
            else
                b1 = 0;

            if( is_pressed( key_map[player].b2 ) )
                b2 = 1;
            else
                b2 = 0;

            if( is_pressed( key_map[player].b3 ) )
                b3 = 1;
            else
                b3 = 0;

            if( is_pressed( key_map[player].b4 ) )
                b4 = 1;
            else
                b4 = 0;
        }
    }
    else
    {
        x = y = b1 = b2 = b3 = 0;
    }
}

void key_bindings(int player, int &left, int &right, int &up, int &down, int &b1, int &b2, int &b3, int &b4)
{
    left = key_map[player].left;
    right = key_map[player].right;
    up = key_map[player].up;
    down = key_map[player].down;
    b1 = key_map[player].b1;
    b2 = key_map[player].b2;
    b3 = key_map[player].b3;
    b3 = key_map[player].b4;
}


void config_cleanup()
{
    if(key_map)
    {
        free(key_map);
        key_map = NULL;
    }
}

//
// Get the keycode for the string 'str'
// Returns -1 for an invalid key code
//
int get_keycode(char const *str)
{
    if( !str[0] )
    {
        return -1;
    }
    else if( !str[1] )
    {
        return str[0];
    }
    else
    {
        int j;
        char buf[20];
        for( j = 256; j < JK_MAX_KEY; j++ )
        {
            key_name( j, buf );
            char *c = buf;
            for( ; *c; c++ )
            {
                if( *c == ' ' )
                {
                    *c = '_';
                }
                else
                {
                    *c = tolower( *c );
                }
            }
            if( strcmp( str, buf ) == 0 )
            {
                return j;
            }
        }
    }
    return -1;
}
