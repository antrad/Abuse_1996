/*
 * Extracting PCX images stored in Abuse SPEC files to modern image formats using OpenCV
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

//
// AR 2016 - settings and list of files to process are stored in "..\abuse-tool\extract.txt"
//

// OpenCV-2.1.0-win32-vs2008
//	- linker
//		cv210.lib
//		cxcore210.lib
//		highgui210.lib
//	- libraries
//		OpenCV2.1\lib
//	- include
//		OpenCV2.1\include
//		OpenCV2.1\include\opencv
//

//OpenCV supported image file formats (! - see more info online):
//	Windows bitmaps				- *.bmp, *.dib			(always supported)
//	JPEG files					- *.jpeg, *.jpg, *.jpe	(!)
//	JPEG 2000 files				- *.jp2					(!)
//	Portable Network Graphics	- *.png					(!)
//	Portable image format		- *.pbm, *.pgm, *.ppm	(always supported)
//	Sun rasters					- *.sr, *.ras			(always supported)
//	TIFF files					- *.tiff, *.tif			(!)

//Abuse image types (transparency):
//	- static/solid image
//		SPEC_IMAGE(4)		- [0,0,0] pixel color should not be transparent (not sure if always)
//  - tilemaps
//		SPEC_FORETILE(5)	- one tilemap per .spe file, every tile same size, [0,0,0] pixel color can be transparent (not sure if always)
//		SPEC_BACKTILE(6)	- one tilemap per .spe file, every tile same size, [0,0,0] pixel color should not be transparent (not sure if always)
//	- animated				
//		SPEC_CHARACTER(7)	- [0,0,0] pixel color should be transparent
//		SPEC_CHARACTER2(21)	- [0,0,0] pixel color should be transparent

#pragma once

#include <cstring>
#include <cstdio>
#include <fstream>
#include <sstream>

#include <opencv2/opencv.hpp>
#include <opencv2/highgui.hpp>
using namespace cv;

#include "common.h"
#include "specs.h"
#include "image.h"
#include "pcxread.h"
#include "crc.h"

#include "AR_Help.h"

enum AR_OpenCV_Stuff
{
	AR_OCV_KEEPCOLOR,		// transparency - keep original values
	AR_OCV_COLORTOALPHA,	// transparency - store in alpha channel (png only)
	AR_OCV_NEWCOLOR,		// transparency - replace color with new values
	AR_OCV_FILEPERPCX,		// output - each image will be stored in separate files
	AR_OCV_FILEPERGROUP,	// output - animations or foregorund/background tilesets will be stored in one file
	AR_OCV_FILEPERSPEC		// output - all the images in spe file will be stored in one file
};

class AR_ImageGroup
{
public:
	int x, y, w, h;
	std::string name;
	std::vector<image*> images;

	AR_ImageGroup()
	{
		this->x = y = w = h = 0;
	}
};

class AR_SPEC
{
public:
	AR_Log	*log;						//global log
	AR_Log	tx_info;					//stores image file names, position of images in a texture atlas, size...

	std::string image_format;
	
	int png_compression;				// 0-9, higher value means a smaller size and longer compression time, OpenCV default is 3
	int jpeg_quality;					// 0-100, higher is better, OpenCV default is 95

	int alpha;							// handling "transparent" black pixels in animated images
	int alpha_r, alpha_g, alpha_b;		// new values for transparent color in animated images [0-255], [0,0,0] is default in Abuse

	int output_tilemap;					//all in one or each tile new file
	int tilemap_rows, tilemap_columns;
	int tilemap_padding;				//padding between individual tiles in pixels
	char tilemap_fill;					//fill rows or fill columns
	std::string tilemap_palette;		//external tile order (edit.lsp)

	int output_animation;				//all in one, group(animations), each frame new file
	int outline_r, outline_g, outline_b;

	std::string color_palette;			//external pallete for tints (art/tints->cop,ant,gun)
	int pal_shift;						//shift by 1 can brighten the image, but can mess up colors in some images

	int group_max;						//form a group every x entries (only for images, sect.spe to be precise)
	bool pong_the_bong;					//pong and bong SPEC files contain tiles and animations and mess everything up, so every tile will be exported as a separate group

	AR_SPEC();
	
	int			AR_ParseConfig		(std::string file_path);	

	bool		AR_ConvertSPEC		(std::string file_path);
	palette*	AR_GetPalette		(std::string file_path);

	void		AR_PrepareGroups	(std::vector<AR_ImageGroup> &groups, int &surface, int &max_width);
	int			AR_GetGroup			(std::string name, std::vector<AR_ImageGroup> &groups);	
	image*		AR_GetImageByName	(std::string name, std::vector<AR_ImageGroup> &groups);//for palette based tile positioning from edit.lsp
	
	bool		AR_CreateTile		(image *im, std::string path, palette *pal, palette *pal_custom);
	bool		AR_CreateImage		(std::vector<std::vector<int> > &m, int w, int h, std::string name, palette *pal, palette *pal_hires, palette *pal_custom);
	bool		AR_SaveImage		(Mat &ocv, std::string path);
};
