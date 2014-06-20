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
#include <stdlib.h>
#include <sys/stat.h>
#include <signal.h>
#include <SDL.h>
#ifdef HAVE_OPENGL
#ifdef __APPLE__
#include <Carbon/Carbon.h>
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#endif    /* __APPLE__ */
#endif    /* HAVE_OPENGL */

#include "specs.h"
#include "keys.h"
#include "setup.h"

flags_struct flags;
keys_struct keys;

extern int xres, yres;
static unsigned int scale;

//
// Display help
//
void showHelp()
{
    printf( "\n" );
    printf( "Usage: abuse.sdl [options]\n" );
    printf( "Options:\n\n" );
    printf( "** Abuse Options **\n" );
    printf( "  -size <arg>       Set the size of the screen\n" );
    printf( "  -edit             Startup in editor mode\n" );
    printf( "  -a <arg>          Use addon named <arg>\n" );
    printf( "  -f <arg>          Load map file named <arg>\n" );
    printf( "  -lisp             Startup in lisp interpreter mode\n" );
    printf( "  -nodelay          Run at maximum speed\n" );
    printf( "\n" );
    printf( "** Abuse-SDL Options **\n" );
    printf( "  -datadir <arg>    Set the location of the game data to <arg>\n" );
    printf( "  -doublebuf        Enable double buffering\n" );
    printf( "  -fullscreen       Enable fullscreen mode\n" );
#ifdef HAVE_OPENGL
    printf( "  -gl               Enable OpenGL\n" );
    printf( "  -antialias        Enable anti-aliasing (with -gl only)\n" );
#endif
    printf( "  -h, --help        Display this text\n" );
    printf( "  -mono             Disable stereo sound\n" );
    printf( "  -nosound          Disable sound\n" );
    printf( "  -scale <arg>      Scale to <arg>\n" );
//    printf( "  -x <arg>          Set the width to <arg>\n" );
//    printf( "  -y <arg>          Set the height to <arg>\n" );
    printf( "\n" );
    printf( "Anthony Kruize <trandor@labyrinth.net.au>\n" );
    printf( "\n" );
}

//
// Create a default 'abuserc' file
//
void createRCFile( char *rcfile )
{
    FILE *fd = NULL;

    if( (fd = fopen( rcfile, "w" )) != NULL )
    {
        fputs( "; Abuse-SDL Configuration file\n\n", fd );
        fputs( "; Startup fullscreen\nfullscreen=0\n\n", fd );
        #ifdef __APPLE__
        fputs( "; Use DoubleBuffering\ndoublebuf=1\n\n", fd );
        fputs( "; Use OpenGL\ngl=1\n\n", fd );
        #else
        fputs( "; Use DoubleBuffering\ndoublebuf=0\n\n", fd );
        fputs( "; Use OpenGL\ngl=0\n\n", fd );
        fputs( "; Location of the datafiles\ndatadir=", fd );
        fputs( ASSETDIR "\n\n", fd );
        #endif
        fputs( "; Use mono audio only\nmono=0\n\n", fd );
        fputs( "; Grab the mouse to the window\ngrabmouse=0\n\n", fd );
        fputs( "; Set the scale factor\nscale=2\n\n", fd );
        fputs( "; Use anti-aliasing (with gl=1 only)\nantialias=1\n\n", fd );
//        fputs( "; Set the width of the window\nx=320\n\n", fd );
//        fputs( "; Set the height of the window\ny=200\n\n", fd );
        fputs( "; Disable the SDL parachute in the case of a crash\nnosdlparachute=0\n\n", fd );
        fputs( "; Key mappings\n", fd );
        fputs( "left=LEFT\nright=RIGHT\nup=UP\ndown=DOWN\n", fd );
        fputs( "fire=SPACE\nweapprev=CTRL_R\nweapnext=INSERT\n", fd );
        fclose( fd );
    }
    else
    {
        printf( "Unable to create 'abuserc' file.\n" );
    }
}

//
// Read in the 'abuserc' file
//
void readRCFile()
{
    FILE *fd = NULL;
    char *rcfile;
    char buf[255];
    char *result;

    rcfile = (char *)malloc( strlen( get_save_filename_prefix() ) + 9 );
    sprintf( rcfile, "%s/abuserc", get_save_filename_prefix() );
    if( (fd = fopen( rcfile, "r" )) != NULL )
    {
        while( fgets( buf, sizeof( buf ), fd ) != NULL )
        {
            result = strtok( buf, "=" );
            if( strcasecmp( result, "fullscreen" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                flags.fullscreen = atoi( result );
            }
            else if( strcasecmp( result, "doublebuf" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                flags.doublebuf = atoi( result );
            }
            else if( strcasecmp( result, "mono" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                flags.mono = atoi( result );
            }
            else if( strcasecmp( result, "grabmouse" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                flags.grabmouse = atoi( result );
            }
            else if( strcasecmp( result, "scale" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                scale = atoi( result );
//                flags.xres = xres * atoi( result );
//                flags.yres = yres * atoi( result );
            }
/*            else if( strcasecmp( result, "x" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                flags.xres = atoi( result );
            }
            else if( strcasecmp( result, "y" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                flags.yres = atoi( result );
            }*/
            else if( strcasecmp( result, "gl" ) == 0 )
            {
                // We leave this in even if we don't have OpenGL so we can
                // at least inform the user.
                result = strtok( NULL, "\n" );
                flags.gl = atoi( result );
            }
#ifdef HAVE_OPENGL
            else if( strcasecmp( result, "antialias" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                if( atoi( result ) )
                {
                    flags.antialias = GL_LINEAR;
                }
            }
#endif
            else if( strcasecmp( result, "nosdlparachute" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                flags.nosdlparachute = atoi( result );
            }
            else if( strcasecmp( result, "datadir" ) == 0 )
            {
                result = strtok( NULL, "\n" );
                set_filename_prefix( result );
            }
            else if( strcasecmp( result, "left" ) == 0 )
            {
                result = strtok( NULL,"\n" );
                keys.left = key_value( result );
            }
            else if( strcasecmp( result, "right" ) == 0 )
            {
                result = strtok( NULL,"\n" );
                keys.right = key_value( result );
            }
            else if( strcasecmp( result, "up" ) == 0 )
            {
                result = strtok( NULL,"\n" );
                keys.up = key_value( result );
            }
            else if( strcasecmp( result, "down" ) == 0 )
            {
                result = strtok( NULL,"\n" );
                keys.down = key_value( result );
            }
            else if( strcasecmp( result, "fire" ) == 0 )
            {
                result = strtok( NULL,"\n" );
                keys.b2 = key_value( result );
            }
            else if( strcasecmp( result, "special" ) == 0 )
            {
                result = strtok( NULL,"\n" );
                keys.b1 = key_value( result );
            }
            else if( strcasecmp( result, "weapprev" ) == 0 )
            {
                result = strtok( NULL,"\n" );
                keys.b3 = key_value( result );
            }
            else if( strcasecmp( result, "weapnext" ) == 0 )
            {
                result = strtok( NULL,"\n" );
                keys.b4 = key_value( result );
            }
        }
        fclose( fd );
    }
    else
    {
        // Couldn't open the abuserc file so let's create a default one
        createRCFile( rcfile );
    }
    free( rcfile );
}

//
// Parse the command-line parameters
//
void parseCommandLine( int argc, char **argv )
{
    for( int ii = 1; ii < argc; ii++ )
    {
        if( !strcasecmp( argv[ii], "-fullscreen" ) )
        {
            flags.fullscreen = 1;
        }
        else if( !strcasecmp( argv[ii], "-doublebuf" ) )
        {
            flags.doublebuf = 1;
        }
        else if( !strcasecmp( argv[ii], "-size" ) )
        {
            if( ii + 1 < argc && !sscanf( argv[++ii], "%d", &xres ) )
            {
                xres = 320;
            }
            if( ii + 1 < argc && !sscanf( argv[++ii], "%d", &yres ) )
            {
                yres = 200;
            }
        }
        else if( !strcasecmp( argv[ii], "-scale" ) )
        {
            int result;
            if( sscanf( argv[++ii], "%d", &result ) )
            {
                scale = result;
/*                flags.xres = xres * scale;
                flags.yres = yres * scale; */
            }
        }
/*        else if( !strcasecmp( argv[ii], "-x" ) )
        {
            int x;
            if( sscanf( argv[++ii], "%d", &x ) )
            {
                flags.xres = x;
            }
        }
        else if( !strcasecmp( argv[ii], "-y" ) )
        {
            int y;
            if( sscanf( argv[++ii], "%d", &y ) )
            {
                flags.yres = y;
            }
        }*/
        else if( !strcasecmp( argv[ii], "-nosound" ) )
        {
            flags.nosound = 1;
        }
        else if( !strcasecmp( argv[ii], "-gl" ) )
        {
            // We leave this in even if we don't have OpenGL so we can
            // at least inform the user.
            flags.gl = 1;
        }
#ifdef HAVE_OPENGL
        else if( !strcasecmp( argv[ii], "-antialias" ) )
        {
            flags.antialias = GL_LINEAR;
        }
#endif
        else if( !strcasecmp( argv[ii], "-mono" ) )
        {
            flags.mono = 1;
        }
        else if( !strcasecmp( argv[ii], "-datadir" ) )
        {
            char datadir[255];
            if( ii + 1 < argc && sscanf( argv[++ii], "%s", datadir ) )
            {
                set_filename_prefix( datadir );
            }
        }
        else if( !strcasecmp( argv[ii], "-h" ) || !strcasecmp( argv[ii], "--help" ) )
        {
            showHelp();
            exit( 0 );
        }
    }
}

//
// Setup SDL and configuration
//
void setup( int argc, char **argv )
{
    // Initialise default settings
    flags.fullscreen        = 0;            // Start in a window
    flags.mono                = 0;            // Enable stereo sound
    flags.nosound            = 0;            // Enable sound
    flags.grabmouse            = 0;            // Don't grab the mouse
    flags.nosdlparachute    = 0;            // SDL error handling
    flags.xres = xres        = 320;            // Default window width
    flags.yres = yres        = 200;            // Default window height
#ifdef __APPLE__
    flags.gl                = 1;            // Use opengl
    flags.doublebuf            = 1;            // Do double buffering
#else
    flags.gl                = 0;            // Don't use opengl
    flags.doublebuf            = 0;            // No double buffering
    #endif
#ifdef HAVE_OPENGL
    flags.antialias            = GL_NEAREST;    // Don't anti-alias
#endif
    keys.up                    = key_value( "UP" );
    keys.down                = key_value( "DOWN" );
    keys.left                = key_value( "LEFT" );
    keys.right                = key_value( "RIGHT" );
    keys.b3                    = key_value( "CTRL_R" );
    keys.b4                    = key_value( "INSERT" );
    scale                    = 2;            // Default scale amount

    // Display our name and version
    printf( "%s %s\n", PACKAGE_NAME, PACKAGE_VERSION );

    // Initialize SDL with video and audio support
    if( SDL_Init( SDL_INIT_VIDEO | SDL_INIT_AUDIO ) < 0 )
    {
        printf( "Unable to initialise SDL : %s\n", SDL_GetError() );
        exit( 1 );
    }
    atexit( SDL_Quit );

    // Set the savegame directory
    char *homedir;
    char *savedir;
    FILE *fd = NULL;

    if( (homedir = getenv( "HOME" )) != NULL )
    {
        savedir = (char *)malloc( strlen( homedir ) + 9 );
        sprintf( savedir, "%s/.abuse/", homedir );
        // Check if we already have a savegame directory
        if( (fd = fopen( savedir, "r" )) == NULL )
        {
            // FIXME: Add some error checking here
            mkdir( savedir, S_IRUSR | S_IWUSR | S_IXUSR );
        }
        else
        {
            fclose( fd );
        }
        set_save_filename_prefix( savedir );
        free( savedir );
    }
    else
    {
        // Warn the user that we couldn't set the savename prefix
        printf( "WARNING: Unable to get $HOME environment variable.\n" );
        printf( "         Savegames will probably fail.\n" );
        // Just use the working directory.
        // Hopefully they have write permissions....
        set_save_filename_prefix( "" );
    }

    // Set the datadir to a default value
    // (The current directory)
    #ifdef __APPLE__
    UInt8 buffer[255];
    CFURLRef bundleurl = CFBundleCopyBundleURL(CFBundleGetMainBundle());
    CFURLRef url = CFURLCreateCopyAppendingPathComponent(kCFAllocatorDefault, bundleurl, CFSTR("Contents/Resources/data"), true);

    if (!CFURLGetFileSystemRepresentation(url, true, buffer, 255))
    {
        exit(1);
    }
    else
        set_filename_prefix( (const char*)buffer );
    #else
    set_filename_prefix( ASSETDIR );
    #endif

    // Load the users configuration
    readRCFile();

    // Handle command-line parameters
    parseCommandLine( argc, argv );

    // Calculate the scaled window size.
    flags.xres = xres * scale;
    flags.yres = yres * scale;

    // Stop SDL handling some errors
    if( flags.nosdlparachute )
    {
        // segmentation faults
        signal( SIGSEGV, SIG_DFL );
        // floating point errors
        signal( SIGFPE, SIG_DFL );
    }
}

//
// Get the key binding for the requested function
//
int get_key_binding(char const *dir, int i)
{
    if( strcasecmp( dir, "left" ) == 0 )
        return keys.left;
    else if( strcasecmp( dir, "right" ) == 0 )
        return keys.right;
    else if( strcasecmp( dir, "up" ) == 0 )
        return keys.up;
    else if( strcasecmp( dir, "down" ) == 0 )
        return keys.down;
    else if( strcasecmp( dir, "b1" ) == 0 )
        return keys.b1;
    else if( strcasecmp( dir, "b2" ) == 0 )
        return keys.b2;
    else if( strcasecmp( dir, "b3" ) == 0 )
        return keys.b3;
    else if( strcasecmp( dir, "b4" ) == 0 )
        return keys.b4;

    return 0;
}
