/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 2001 Anthony Kruize <trandor@labyrinth.net.au>
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *  Copyright (c) 2016 Antonio Radojkovic <antonior.software@gmail.com>
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
# include <ShlObj.h>
# include <direct.h>
# define strcasecmp _stricmp
#endif
#ifdef __APPLE__
# include <CoreFoundation/CoreFoundation.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <signal.h>
#include <string>
#include "SDL.h"

#include "specs.h"
#include "keys.h"
#include "setup.h"
#include "errorui.h"

//AR
#include <fstream>
#include <sstream>
extern Settings settings;
//

extern int xres, yres;					//video.cpp
extern int sfx_volume, music_volume;	//loader.cpp
unsigned int scale;						//AR was static, removed for external

//AR tmp, until I figure out compiling stuff and cmake...
int AR_ToInt(std::string value)
{
	int n = 1;

	std::stringstream stream(value);
	stream >> n;

	return n;
}

bool AR_ToBool(std::string value)
{
	bool n = false;

	std::stringstream stream(value);
	stream >> n;

	return n;
}

bool AR_GetAttr(std::string line, std::string &attr, std::string &value)
{
	attr = value = "";

	std::size_t found = line.find("=");

	//no "="
	if(found==std::string::npos || found==line.size()-1) return false;
	
	attr = line.substr(0,found);
	value = line.substr(found+1,line.size()-1);

	//empty attribute or value
	if(attr.empty() || value.empty()) return false;
	
	return true;
}
//

Settings::Settings()
{
	//screen
	this->fullscreen		= false;    // Start in window (fullscreen is actually windowed-fullscreen now)
	this->vsync				= false;
	this->xres				= 640;		// Default window width
	this->yres				= 400;		// Default window height
	this->scale				= 1;		// default window scale
	this->linear_filter		= false;    // Don't "anti-alias"		

	//sound
	this->mono				= false;	// Enable stereo sound
	this->no_sound			= false;	// Enable sound
	this->no_music			= false;	// Enable music	
	this->volume_sound		= 127;
	this->volume_music		= 127;

	//random
	this->local_save		= true;
	this->grab_mouse		= false;	// Don't grab the mouse
	this->editor			= false;	// disable editor mode
	this->physics_update	= 50;		// 20 FPS (original 65ms/15 FPS)
	this->mouse_scale		= 0;		// match desktop
	
	//player controls
	this->up		= key_value("w");
    this->down		= key_value("s");
    this->left		= key_value("a");
    this->right		= key_value("d");
    this->up_2		= key_value("UP");
    this->down_2	= key_value("DOWN");
    this->left_2	= key_value("LEFT");
    this->right_2	= key_value("RIGHT");
	this->b1		= key_value("SHIFT_L");	//special
	this->b2		= key_value("f");		//fire
    this->b3		= key_value("q");		//weapons
    this->b4		= key_value("e");

	//controller settings
	this->ctr_aim		= false;	// controller overide disabled
	this->ctr_cd		= 100;
	this->ctr_rst_s		= 10;
	this->ctr_rst_dz	= 5000;		// aiming
	this->ctr_lst_dzx	= 10000;	// move left right
	this->ctr_lst_dzy	= 25000;	// up/jump, down/use
	this->ctr_aim_x		= 0;
	this->ctr_aim_y		= 0;
	this->ctr_mouse_x	= 0;
	this->ctr_mouse_y	= 0;

	//controller buttons
	this->ctr_a = "up";
	this->ctr_b = "down";
	this->ctr_x = "b3";
	this->ctr_y = "b4";
	//
	this->ctr_lst = "b1";
	this->ctr_rst = "down";
	//
	this->ctr_lsr = "b2";
	this->ctr_rsr = "b3";
	//
	this->ctr_ltg = "b1";
	this->ctr_rtg = "b2";
}

//////////
////////// CREATE DEFAULT "config.txt" FILE
//////////

bool Settings::CreateConfigFile(std::string file_path)
{
	std::ofstream out(file_path.c_str());
	if(!out.is_open())
	{
		std::string tmp = "ERROR - CreateConfigFile() - Failed to create \"" + file_path + "\"\n";
		printf(tmp.c_str());

		return false;
	}
	
	out << "; Abuse-SDL configuration file (v0.9a)" << std::endl;
	out << std::endl;
	//
	out << "; SCREEN SETTINGS" << std::endl;
	out << std::endl;
	out << "fullscreen=" << this->fullscreen << std::endl;
	out << "vsync=" << this->vsync << std::endl;
	out << std::endl;
	out << "; Game screen size (original 320x200)" << std::endl;
	out << "screen_width=" << this->xres << std::endl;
	out << "screen_height=" << this->yres << std::endl;
	out << std::endl;
	out << "; Scale window" << std::endl;
	out << "scale=" << this->scale << std::endl;
	out << std::endl;
	out << "; Use linear texture filter (nearest is default)" << std::endl;
	out << "linear_filter=" << this->linear_filter << std::endl;
	out << std::endl;
	//
	out << "; SOUND SETTINGS" << std::endl;
	out << std::endl;
	out << "; Volume (0-127)" << std::endl;
	out << "volume_sound=" << this->volume_sound << std::endl;
	out << "volume_music=" << this->volume_music << std::endl;
	out << std::endl;
	out << "; Use mono audio only" << std::endl;
	out << "mono=" << this->mono << std::endl;
	out << std::endl;
	out << "; Disable music" << std::endl;
	out << "no_music=" << this->no_music << std::endl;
	out << std::endl;
	out << "; Disable sound effects" << std::endl;
	out << "no_sound=" << this->no_sound << std::endl;
	out << std::endl;
	//
	out << "; RANDOM SETTINGS" << std::endl;
	out << std::endl;
	out << "local_save=" << this->local_save << std::endl;	
	out << std::endl;
	out << "; Grab the mouse to the window" << std::endl;
	out << "grab_mouse=" << this->grab_mouse << std::endl;
	out << std::endl;
	out << "; Enable editor mode" << std::endl;
	out << "editor=" << this->editor << std::endl;
	out << std::endl;
	out << "; Physics update time in ms (65ms/15FPS original, 50ms/20FPS recommended on higher resolutions)" << std::endl;
	out << "physics_update=" << this->physics_update << std::endl;
	out << std::endl;
	out << "; Fullscreen mouse scaling (0 - match desktop, 1 - match game screen)" << std::endl;
	out << "mouse_scale=" << this->mouse_scale << std::endl;
	out << std::endl;	
	//
	out << "; PLAYER CONTROLS" << std::endl;
	out << std::endl;
	out << "; Key mappings" << std::endl;
	out << "left=a" << std::endl;
	out << "right=d" << std::endl;
	out << "up=w" << std::endl;
	out << "down=s" << std::endl;
	out << "special=SHIFT_L" << std::endl;
	out << "fire=f" << std::endl;
	out << "weapon_prev=q" << std::endl;
	out << "weapon_next=e" << std::endl;	
	out << std::endl;
	//
	out << "; Alternative key mappings (only the following controls can have two keyboard bindings)" << std::endl;
	out << "left_2=LEFT" << std::endl;
	out << "right_2=RIGHT" << std::endl;
	out << "up_2=UP" << std::endl;
	out << "down_2=DOWN" << std::endl;	
	out << std::endl;
	//
	out << "; CONTROLLER SETTINGS" << std::endl;
	out << std::endl;
	out << "; Enable aiming" << std::endl;
	out << "ctr_aim=" << this->ctr_aim << std::endl;
	out << std::endl;
	out << "; Crosshair distance from player" << std::endl;
	out << "ctr_cd=" << this->ctr_cd << std::endl;
	out << std::endl;
	out << "; Right stick/aiming sensitivity" << std::endl;
	out << "ctr_rst_s=" << this->ctr_rst_s << std::endl;
	out << std::endl;
	out << "; Right stick/aiming dead zone" << std::endl;
	out << "ctr_rst_dz=" << this->ctr_rst_dz << std::endl;
	out << std::endl;
	out << "; Left stick/movement dead zones" << std::endl;
	out << "ctr_lst_dzx=" << this->ctr_lst_dzx << std::endl;
	out << "ctr_lst_dzy=" << this->ctr_lst_dzy << std::endl;
	out << std::endl;	
	//
	out << "; Button mappings (don't use buttons for left/right movement)" << std::endl;
	//ovo bi treba u readme stavit
	/*out << "; ctr_a, ctr_b, ctr_x, ctr_y" << std::endl;
	out << "; ctr_left_stick, ctr_right_stick" << std::endl;
	out << "; ctr_left_shoulder, ctr_right_shoulder" << std::endl;
	out << "; ctr_left_trigger, ctr_right_trigger" << std::endl;*/
	out << "up=ctr_a" << std::endl;	
	out << "down=ctr_b" << std::endl;
	out << "special=ctr_left_shoulder" << std::endl;
	out << "special=ctr_left_trigger" << std::endl;
	out << "special=ctr_left_stick" << std::endl;
	out << "fire=ctr_right_shoulder" << std::endl;
	out << "fire=ctr_right_trigger" << std::endl;
	out << "fire=ctr_right_stick" << std::endl;
	out << "weapon_prev=ctr_x" << std::endl;
	out << "weapon_next=ctr_y" << std::endl;
	
	out.close();

	printf("Default \"config.txt\" created\n");
	
	return true;

	/*
	#if !((defined __APPLE__) || (defined WIN32))
	fputs( "; Location of the datafiles\ndatadir=", fd );
	fputs( ASSETDIR "\n\n", fd );
	#endif	
	*/
}

//////////
////////// READ CONFIG FILE
//////////

bool Settings::ReadConfigFile(std::string folder)
{
	std::string file_path = folder + "config.txt";

	std::ifstream filein(file_path.c_str());
	if(!filein.is_open())
	{
		std::string tmp = "ERROR - ReadConfigFile() - Failed to open \"" + file_path + "\"\n";
		printf(tmp.c_str());

		//try to create it
		return CreateConfigFile(file_path);
	}

	std::string line;
	while(std::getline(filein,line))
	{
		//stop reading file
		if(line=="exit")
		{
			filein.close();
			return true;
		}

		//skip empty line or ";" which marks a comment
		if(line.empty() || line[0]==';') continue;

		std::string attr, value;

		//quit if bad command
		if(!AR_GetAttr(line,attr,value))
		{
			filein.close();

			std::string tmp = "ERROR - ReadConfigFile() - Bad command \"" + line + "\"\n";
			printf(tmp.c_str());			

			return CreateConfigFile(file_path);
		}

		//screen
		if(attr=="fullscreen")				this->fullscreen = AR_ToBool(value);
		else if(attr=="vsync")				this->vsync = AR_ToBool(value);
		else if(attr=="screen_width")		this->xres = AR_ToInt(value);
		else if(attr=="screen_height")		this->yres = AR_ToInt(value);
		else if(attr=="scale")				this->scale = AR_ToInt(value);
		else if(attr=="linear_filter")		this->linear_filter = AR_ToBool(value);

		//sound
		else if(attr=="mono")				this->mono = AR_ToBool(value);
		else if(attr=="no_sound")			this->no_sound = AR_ToBool(value);
		else if(attr=="no_music")			this->no_music = AR_ToBool(value);
		else if(attr=="volume_sound")
		{
			this->volume_sound = AR_ToInt(value);
			sfx_volume = this->volume_sound;
		}
		else if(attr=="volume_music")
		{
			this->volume_music = AR_ToInt(value);
			music_volume = this->volume_music;
		}

		//random
		else if(attr=="local_save")			this->local_save = AR_ToBool(value);
		else if(attr=="grab_mouse")			this->grab_mouse = AR_ToBool(value);
		else if(attr=="editor")				this->editor = AR_ToBool(value);
		else if(attr=="physics_update")		this->physics_update = AR_ToInt(value);
		else if(attr=="mouse_scale")		this->mouse_scale = AR_ToInt(value);
		
		//player controls
		else if(attr=="up")
		{
			if(!ControllerButton(attr,value))	this->up = key_value(value.c_str());
		}
		else if(attr=="down")
		{
			if(!ControllerButton(attr,value))	this->down = key_value(value.c_str());
		}
		else if(attr=="left")
		{
			if(!ControllerButton(attr,value))	this->left = key_value(value.c_str());
		}
		else if(attr=="right")
		{
			if(!ControllerButton(attr,value))	this->right = key_value(value.c_str());
		}		
		//
		else if(attr=="special")
		{
			if(!ControllerButton(attr,value))	this->b1 = key_value(value.c_str());
		}
		else if(attr=="fire")
		{
			if(!ControllerButton(attr,value))	this->b2 = key_value(value.c_str());
		}
		else if(attr=="weapon_prev")
		{
			if(!ControllerButton(attr,value))	this->b3 = key_value(value.c_str());
		}
		else if(attr=="weapon_next")
		{
			if(!ControllerButton(attr,value)) this->b4 = key_value(value.c_str());
		}
		//
		else if(attr=="up_2")			this->up_2 = key_value(value.c_str());
		else if(attr=="down_2")			this->down_2 = key_value(value.c_str());
		else if(attr=="left_2")			this->left_2 = key_value(value.c_str());
		else if(attr=="right_2")		this->right_2 = key_value(value.c_str());
		
		//controller settings
		else if(attr=="ctr_aim")		this->ctr_aim = AR_ToBool(value);
		else if(attr=="ctr_cd")			this->ctr_cd = AR_ToInt(value);
		else if(attr=="ctr_rst_s")		this->ctr_rst_s = AR_ToInt(value);
		else if(attr=="ctr_rst_dz")		this->ctr_rst_dz = AR_ToInt(value);
		else if(attr=="ctr_lst_dzx")	this->ctr_lst_dzx = AR_ToInt(value);
		else if(attr=="ctr_lst_dzy")	this->ctr_lst_dzy = AR_ToInt(value);
		else
		{
			filein.close();
			
			std::string tmp = "ERROR - ReadConfigFile() - Bad command \"" + line + "\"\n";
			printf(tmp.c_str());
			
			return CreateConfigFile(file_path);
		}
	}

	filein.close();

	return true;

	/*
	if( strcasecmp( result, "datadir" ) == 0 )
	{
	result = strtok( NULL, "\n" );
	set_filename_prefix( result );
	}
	*/
}

bool Settings::ControllerButton(std::string c, std::string b)
{
	std::string control = c;

	if(c=="special")			control = "b1";
	else if(c=="fire")			control = "b2";
	else if(c=="weapon_prev")	control = "b3";
	else if(c=="weapon_next")	control = "b4";

	if(b=="ctr_a") {this->ctr_a = control;return true;};
	if(b=="ctr_b") {this->ctr_b = control;return true;};
	if(b=="ctr_x") {this->ctr_x = control;return true;};
	if(b=="ctr_y") {this->ctr_y = control;return true;};
	//
	if(b=="ctr_left_stick")		{this->ctr_lst = control;return true;};
	if(b=="ctr_right_stick")	{this->ctr_rst = control;return true;};
	//
	if(b=="ctr_left_shoulder")	{this->ctr_lsr = control;return true;};
	if(b=="ctr_right_shoulder")	{this->ctr_rsr = control;return true;};
	//
	if(b=="ctr_left_trigger")	{this->ctr_ltg = control;return true;};
	if(b=="ctr_right_trigger")	{this->ctr_rtg = control;return true;};

	return false;
}

//
// Display help
//
void showHelp(const char* executableName)
{
    printf( "\n" );
    printf( "Usage: %s [options]\n", executableName );
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
    printf( "  -fullscreen       Enable fullscreen mode\n" );
    printf( "  -antialias        Enable anti-aliasing\n" );
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
// Parse the command-line parameters
//
void parseCommandLine(int argc, char **argv)
{
	//AR this is called before settings.ReadConfigFile(), so I can override stuff via console

	for(int i=1;i<argc;i++)
	{
		if(!strcasecmp(argv[i],"-remote_save"))
		{
			settings.local_save = false;
		}
	}
}

//
// Setup SDL and configuration
//
void setup( int argc, char **argv )
{
	// Display our name and version
    printf( "%s %s\n", PACKAGE_NAME, PACKAGE_VERSION );

    // Initialize SDL with video and audio support
    if( SDL_Init( SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_JOYSTICK | SDL_INIT_GAMECONTROLLER ) < 0 )
    {
        show_startup_error( "Unable to initialize SDL : %s\n", SDL_GetError() );
        exit( 1 );
    }
    atexit( SDL_Quit );

    // Set the savegame directory
    char *homedir;
    char *savedir;
    FILE *fd = NULL;

#ifdef WIN32
    // Grab the profile dir
    PWSTR appData;
    SHGetKnownFolderPath(FOLDERID_RoamingAppData, 0, NULL, &appData);
    // Create a new chunk of memory to save the savedir in
    size_t savedir_size = lstrlenW(appData) * 2 + 7;
    savedir = (char*) malloc(savedir_size);
    wcstombs(savedir, appData, savedir_size);
    // Append "\Abuse\" to end end of it
    strcat(savedir, "\\Abuse\\");
    // If it doesn't exist, create it
    if ( (fd = fopen(savedir, "r")) == NULL) {
        // FIXME: Add some error checking here
        _mkdir(savedir);
    } else {
        fclose( fd );
    }
    set_save_filename_prefix(savedir);
    CoTaskMemFree(appData);
    free( savedir );
#else
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
#endif

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
    {
        printf("Setting prefix to [%s]\n", buffer);
        set_filename_prefix( (const char*)buffer );
    }
#elif defined WIN32
    // Under Windows, it makes far more sense to assume the data is stored
    // relative to our executable than anywhere else.
    char assetDirName[MAX_PATH];
    GetModuleFileName(NULL, assetDirName, MAX_PATH);
    // Find the first \ or / and cut the path there
    size_t cut_at = -1;
    for (size_t i = 0; assetDirName[i] != '\0'; i++) {
        if (assetDirName[i] == '\\' || assetDirName[i] == '/') {
            cut_at = i;
        }
    }
    if (cut_at >= 0)
        assetDirName[cut_at] = '\0';
    printf("Setting data dir to %s\n", assetDirName);
    set_filename_prefix( assetDirName );
#else
    set_filename_prefix( ASSETDIR );
#endif

	//handle command-line parameters
    parseCommandLine(argc,argv);

	//AR override save game directory to local directory
	if(settings.local_save) set_save_filename_prefix("user/");

	printf("Setting save dir to %s\n", get_save_filename_prefix());

    // Load the users configuration
    settings.ReadConfigFile(get_save_filename_prefix());

	// Initialize default settings
	scale = settings.scale;
	xres = settings.xres;
	yres = settings.yres;
}

//
// Get the key binding for the requested function
//
int get_key_binding(char const *dir, int i)
{
	if(strcasecmp(dir,"left")==0)			return settings.left;
	else if(strcasecmp(dir,"right")==0)		return settings.right;
	else if(strcasecmp(dir,"up")==0)		return settings.up;
	else if(strcasecmp(dir,"down")==0)		return settings.down;
	else if(strcasecmp(dir,"left2")==0)		return settings.left_2;
	else if(strcasecmp(dir,"right2")==0)	return settings.right_2;
	else if(strcasecmp(dir,"up2")==0)		return settings.up_2;
	else if(strcasecmp(dir,"down2")==0)		return settings.down_2;
	else if(strcasecmp(dir,"b1")==0)		return settings.b1;
	else if(strcasecmp(dir,"b2")==0)		return settings.b2;
	else if(strcasecmp(dir,"b3")==0)		return settings.b3;
	else if(strcasecmp(dir,"b4")==0)		return settings.b4;

	return 0;
}

//AR controller
std::string get_ctr_binding(std::string c)
{
    if(c=="ctr_a")			return settings.ctr_a;
	else if(c=="ctr_b")		return settings.ctr_b;
	else if(c=="ctr_x")		return settings.ctr_x;
	else if(c=="ctr_y")		return settings.ctr_y;
	//
	else if(c=="ctr_lst")	return settings.ctr_lst;
	else if(c=="ctr_rst")	return settings.ctr_rst;
	//
	else if(c=="ctr_lsr")	return settings.ctr_lsr;
	else if(c=="ctr_rsh")	return settings.ctr_rsr;
	//
	else if(c=="ctr_ltg")	return settings.ctr_ltg;
	else if(c=="ctr_rtg")	return settings.ctr_rtg;
	
	return "";
}
