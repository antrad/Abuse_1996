//
// The MIT License (MIT)
//
// Extracting PCX images stored in Abuse SPEC files to modern image formats using OpenCV
//
// Copyright (c) 2016 Antonio Radojkovic <antonior.software@gmail.com>

//	Permission is hereby granted, free of charge, to any person obtaining a copy of this software
//	and associated documentation files (the "Software"), to deal in the Software without
//	restriction, including without limitation the rights to use, copy, modify, merge, publish,
//	distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom
//	the Software is furnished to do so, subject to the following conditions:

//	The above copyright notice and this permission notice shall be included in all copies or
//	substantial portions of the Software.

//	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
//	FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
//	OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
//	WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
//	IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//

#include "AR_SPEC.h"

AR_SPEC::AR_SPEC()
{
	this->log = NULL;
	this->tx_info.out_path = "abuse-tool/texture_info.txt";

	this->image_format = "png";

	this->png_compression = 9;
	this->jpeg_quality = 100;

	this->alpha = AR_OCV_KEEPCOLOR;
	this->alpha_r = alpha_g = alpha_b = 0;

	this->output_tilemap = AR_OCV_FILEPERSPEC;
	this->tilemap_rows = 20;
	this->tilemap_columns = 5;
	this->tilemap_padding = 1;
	this->tilemap_fill = 'r';
	this->tilemap_palette;

	this->output_animation = AR_OCV_FILEPERSPEC;
	this->outline_r = outline_g = outline_b = 128; //default gray
	
	this->pal_shift = 0;

	this->group_max = 0;
	this->pong_the_bong = false;
};

//////////
////////// READ CONFIG FILE AND EXTRACTION LIST
//////////

int AR_SPEC::AR_ParseConfig(std::string file_path)
{
	this->log->Write("\n\nUsage 2: abuse-tool");
	this->log->Write("-------------------\n");

	std::ifstream filein(file_path.c_str());
	if(!filein.is_open())
	{
		this->log->Write("\nERROR - AR_ParseConfig() - Failed to open \"" + file_path + "\"\n");
		return EXIT_FAILURE;
	}

	std::string line;
	while(std::getline(filein,line))
	{
		//stop reading file
		if(line=="exit") return EXIT_SUCCESS;

		//skip empty line or ";" which marks a comment
		if(line.empty() || line[0]==';') continue;

		std::string attr, value;

		//quit if bad command
		if(!AR_GetAttr(line,attr,value)) return EXIT_FAILURE;

		if(attr=="this->image_format")			this->image_format = value;
		else if(attr=="this->png_compression")	this->png_compression = AR_ToInt(value);
		else if(attr=="this->jpeg_quality")		this->jpeg_quality = AR_ToInt(value);
		else if(attr=="this->alpha")			this->alpha = AR_ToInt(value);
		else if(attr=="this->alpha_r")			this->alpha_r = AR_ToInt(value);
		else if(attr=="this->alpha_g")			this->alpha_g = AR_ToInt(value);
		else if(attr=="this->alpha_b")			this->alpha_b = AR_ToInt(value);
		else if(attr=="this->output_tilemap")	this->output_tilemap = AR_ToInt(value);		
		else if(attr=="this->tilemap_rows")		this->tilemap_rows = AR_ToInt(value);
		else if(attr=="this->tilemap_columns")	this->tilemap_columns = AR_ToInt(value);
		else if(attr=="this->tilemap_padding")	this->tilemap_padding = AR_ToInt(value);
		else if(attr=="this->tilemap_palette")
		{
			if(value=="0") this->tilemap_palette = "";
			else this->tilemap_palette = value;
		}
		else if(attr=="this->tilemap_fill")		this->tilemap_fill = value[0];
		else if(attr=="this->output_animation")	this->output_animation = AR_ToInt(value);
		else if(attr=="this->outline_r")		this->outline_r = AR_ToInt(value);
		else if(attr=="this->outline_g")		this->outline_g = AR_ToInt(value);
		else if(attr=="this->outline_b")		this->outline_b = AR_ToInt(value);
		else if(attr=="this->group_max")		this->group_max = AR_ToInt(value);
		else if(attr=="this->pong_the_bong")	this->pong_the_bong = (bool)AR_ToInt(value);
		else if(attr=="palette_shift")			this->pal_shift = AR_ToInt(value);
		else if(attr=="color_palette")
		{
			if(value=="0") color_palette = "";
			else color_palette = value;
		}		
		else if(attr=="in")
		{
			//convert .pcx files inside .spe to modern image formats
			if(AR_ConvertSPEC(value))
				this->log->Write("\nAR_ConvertSPEC().....\"" + value + "\"..... OK");
			else
			{
				this->log->Write("\nAR_ConvertSPEC().....\"" + value + "\"..... FAILED");				
				return EXIT_FAILURE;
			}
		}
		else
		{
			this->log->Write("\nERROR - AR_GetAttr() - bad command \"" + line + "\"\n");
			return EXIT_FAILURE;
		}
	}

	return EXIT_SUCCESS;
}

bool AR_SPEC::AR_GetAttr(std::string line, std::string &attr, std::string &value)
{
	attr = value = "";

	std::size_t found = line.find("=");

	if(found==std::string::npos || found==line.size()-1)
	{
		this->log->Write("\nERROR - AR_GetAttr() - bad command \"" + line + "\"\n");
		return false;
	}

	attr = line.substr(0,found);
	value = line.substr(found+1,line.size()-1);

	if(attr.empty() || value.empty())
	{
		this->log->Write("\nERROR - AR_GetAttr() - bad command \"" + line + "\"\n");
		return false;
	}

	return true;
}

//////////
////////// EXTRACT IMAGE
//////////

bool AR_SPEC::AR_ConvertSPEC(std::string file_path)
{
	this->log->Write("Reading SPEC \"" + file_path + "\"...");

	//get custom pallete
	palette *pal_custom = NULL;
	if(!color_palette.empty())
	{
		pal_custom = AR_GetPalette(color_palette);
		if(!pal_custom) return false;
	}	

	jFILE fp(file_path.c_str(),"rb");
	if(fp.open_failure())
	{
		this->log->Write("\nERROR - AR_ConvertSPEC() - could not open \"" + file_path + "\"\n");
		return false;
	}
	
	//contains all the images inside the file, groups them if required	
	std::vector<AR_ImageGroup> groups(1);//init with "all in one" group
	groups[0].name = file_path;
	std::size_t found = groups[0].name.find(".spe");
	if(found!=std::string::npos) groups[0].name.erase(found,4);	

	palette *pal = new palette(256);
	palette *pal_hires = NULL;//title.spe has a palette for high resolution images

	//read file entries
	spec_directory dir(&fp);
	dir.FullyLoad(&fp);

	bool in_tiles = false, in_images = false;

	//find palletes
	for(int i=0;i<dir.total;i++)
	{
		spec_entry *se = dir.entries[i];
		
		if(se->type==SPEC_PALETTE)
		{
			std::stringstream stream;
			stream << i << " " << (int)se->type << " " << se->name;
			this->log->Write("Loading palette \"" + stream.str() + "\"...");
			
			std::string name = se->name;
			std::size_t found = name.find("_hires");

			if(found!=std::string::npos)
			{
				if(pal_hires) delete pal_hires;
				pal_hires = new palette(se,&fp);
			}
			else
			{
				if(pal) delete pal;
				pal = new palette(se,&fp);
			}
		}
	}

	this->tx_info.Write("SPEC_file=" + file_path);
	if(pal_custom) this->tx_info.Write("color_palette=" + color_palette);
	
	if(this->pal_shift>0)
	{
		if(pal)			pal->shift(this->pal_shift);
		if(pal_hires)	pal_hires->shift(this->pal_shift);
		if(pal_custom)	pal_custom->shift(this->pal_shift);

		std::stringstream stream;
		stream << "palette_shift=" << this->pal_shift;
		this->tx_info.Write(stream.str());
	}
	
	//find images
	for(int i=0;i<dir.total;i++)
	{
		spec_entry *se = dir.entries[i];		

		if(
			se->type==SPEC_FORETILE ||
			se->type==SPEC_BACKTILE ||
			se->type==SPEC_IMAGE ||
			se->type==SPEC_CHARACTER ||
			se->type==SPEC_CHARACTER2
			)
		{
			std::stringstream stream;
			stream << i << " " << (int)se->type << " " << se->name;
			this->log->Write("Loading image \"" + stream.str() + "\"...");

			image *im = new image(&fp,se);
			im->ar_type = se->type;
			im->ar_name_o = se->name;

			//remove extension
			std::string name = se->name;
			std::size_t found = name.find(".pcx");
			if(found!=std::string::npos) name.erase(found,4);//some images have "_hires" after ".pcx", so we just remove ".pcx"
			im->ar_name = name;

			if(se->type==SPEC_FORETILE || se->type==SPEC_BACKTILE)
			{
				if(!this->pong_the_bong)
				{
					if(this->output_tilemap==AR_OCV_FILEPERPCX)
					{
						if(!in_tiles)
						{
							this->tx_info.Write(";name out width height");
							this->tx_info.Write(";--------------------------------------------");
						}

						palette *pal_fin = pal;
						if(pal_hires) pal_fin = pal_hires;

						bool result = AR_CreateTile(im,groups[0].name + "_" + im->ar_name,pal_fin,pal_custom);
						delete im;
						if(!result) return false;
					}
					else groups[0].images.push_back(im);//put all in the same group

					in_tiles = true;
				}
				else
				{
					//each tile in new group for these 2 special cases
					AR_ImageGroup g;
					g.name = name;
					g.images.push_back(im);
					groups.push_back(g);
				}
			}
			else if(se->type==SPEC_IMAGE || se->type==SPEC_CHARACTER || se->type==SPEC_CHARACTER2)
			{
				if(this->output_animation==AR_OCV_FILEPERPCX)
				{
					if(!in_images)
					{
						this->tx_info.Write(";name out width height");
						this->tx_info.Write(";--------------------------------------------");
					}
					
					palette *pal_fin = pal;
					if(pal_hires) pal_fin = pal_hires;

					bool result = AR_CreateTile(im,groups[0].name + "_" + im->ar_name,pal_fin,pal_custom);
					delete im;
					if(!result) return false;

					in_images = true;
				}
				else if(this->output_animation==AR_OCV_FILEPERGROUP || this->output_animation==AR_OCV_FILEPERSPEC)
				{
					if(this->group_max>0)
					{
						int index = -1;
						for(unsigned int j=0;j<groups.size();j++)
							if(groups[j].images.size()<this->group_max)
							{
								index = j;
								break;
							}

							if(index==-1)
							{
								//all groups full, create new one
								AR_ImageGroup g;
								g.name = name;
								g.images.push_back(im);
								groups.push_back(g);
							}
							else
							{
								if(groups[index].images.empty()) groups[index].name = name;//for all in one group
								groups[index].images.push_back(im);
							}
					}
					else
					{
						//check if it's a animation (those usually have number 00xx in their name)
						std::size_t found = name.find("00");
						if(found!=std::string::npos)
						{
							name.erase(found,4);//remove 00xx number
							int group_index = AR_GetGroup(name,groups);
							if(group_index==-1)
							{
								//create new group
								AR_ImageGroup g;
								g.name = name;
								g.images.push_back(im);
								groups.push_back(g);
							}
							else groups[group_index].images.push_back(im);
						}
						else
						{
							//each image that is not an animatio frame is a new group, so we don't complicate things
							AR_ImageGroup g;
							g.name = name;
							g.images.push_back(im);
							groups.push_back(g);
						}
					}
				}
			}
		}
	}

	if(in_tiles)
	{
		//SPEC_FORETILE, SPEC_BACKTILE - save all to one tilemap, tilemaps have no hires or tint palettes

		if(this->output_tilemap==AR_OCV_FILEPERSPEC)
		{
			std::stringstream tiles;			

			int width_fin = 0, height_fin = 0;

			std::vector<image*> images = groups[0].images;//readibility

			std::stringstream palette_stream(this->tilemap_palette);//custom tile positioning from edit.lsp
			int tile_r = this->tilemap_rows, tile_c = this->tilemap_columns;

			int w = images[0]->Size().x;
			int h = images[0]->Size().y;

			//if not defined, calculate number of rows/columns, w==h
			if(tile_r==-1 && tile_r==-1)
			{
				int rect = sqrt((float)images.size());
				if(rect*rect<images.size()) rect++;
				tile_r = tile_c = rect;
			}

			//calculate output image size
			int width = tile_c*images[0]->Size().x + this->tilemap_padding*(tile_c-1);
			int height = tile_r*images[0]->Size().y + this->tilemap_padding*(tile_r-1);			

			//store pixel values in temporary matrix
			std::vector<std::vector<int> > matrix(height,std::vector<int>(width));

			int posx = 0, posy = 0;
			int columns_fin = 0, rows_fin = 0;//count rows and columns

			int k = 0;//image index
			for(int i=0;i<tile_r;i++)//row
			{
				//posx and posy is the position where the top-left corner of the tile will be positioned in the final output
				posy = i*(h+this->tilemap_padding);

				for(int j=0;j<tile_c;j++)//column
				{
					//image index/counter
					if(this->tilemap_fill=='r') k = i*tile_c + j;
					else k = j*tile_r + i;

					image *im = NULL;

					if(!this->tilemap_palette.empty())
					{
						//0 means no image on that position
						std::string ps;
						if(palette_stream >> ps) im = AR_GetImageByName(ps,groups);
						//else error;//TODO
					}
					else if(k<images.size()) im = images[k];

					if(im)
					{
						bool pixel_alpha = true;
						if(im->ar_type==SPEC_BACKTILE) pixel_alpha = false;

						posx = j*(w+this->tilemap_padding);

						tiles << im->ar_name_o << " ";
						tiles << posx << " " << posy << " ";
						tiles << im->Size().x << " " << im->Size().y << std::endl;

						for(int m=0;m<im->Size().y;m++)//row
							for(int n=0;n<im->Size().x;n++)//column
							{
								int pixel = im->AR_GetPixels()[m*im->Size().x + n];

								//SPEC file can have images with different this->alpha settings
								//so we mark the this->alpha disabled/solid ones with a +10000 value in the output matrix
								//this doesn't apply to tiles, but I don't want to make a new AR_CreatImage()
								if(!pixel_alpha) pixel += 10000;

								matrix[posy+m][posx+n] = pixel;
							}

							//final size of output image
							if(posx + w > width_fin)	width_fin = posx + w;
							if(posy + h > height_fin)	height_fin = posy + h;

							//count rows and columns
							if(j+1 > columns_fin)	columns_fin = j + 1;
							if(i+1 > rows_fin)		rows_fin = i + 1;
							
							this->log->Write(im->ar_name);
					}
				}
			}

			if(!AR_CreateImage(matrix,width_fin,height_fin,file_path.substr(0,file_path.size()-4),pal,pal_hires,pal_custom))
				return false;

			std::stringstream stream;

			if(pal_custom) stream << "out=" << file_path.substr(0,file_path.size()-4) << "_" + AR_GetFileName(color_palette) << "." << this->image_format << std::endl;
			else stream << "out=" << file_path.substr(0,file_path.size()-4) << "." << this->image_format << std::endl;

			stream << ";out_w out_h tile_w tile_h tile_num columns rows padding" << std::endl;
			stream << width_fin << " " << height_fin << " " << w << " " << h << " " << images.size() << " ";
			stream << columns_fin << " " << rows_fin << " " << this->tilemap_padding;
			stream << "\n;------------------------------------------------------------";
			this->tx_info.Write(stream.str());

			this->tx_info.Write(";name x y w h");
			this->tx_info.Write(";--------------------------");
			this->tx_info.Write(tiles.str());
		}
	}
	else
	{
		//form animation groups

		if(this->output_animation==AR_OCV_FILEPERGROUP || this->output_animation==AR_OCV_FILEPERSPEC)
		{
			//count the number of images and groups
			int g_num = 0, im_num = 0;
			for(unsigned int i=0;i<groups.size();i++)
			{
				if(!groups[i].images.empty())//unused all in one group
				{
					g_num++;
					for(unsigned int j=0;j<groups[i].images.size();j++) im_num++;
				}
			}

			std::stringstream stream;
			if(this->output_animation==AR_OCV_FILEPERSPEC)
			{
				if(pal_custom)	stream << "out=" << file_path.substr(0,file_path.size()-4) + "_" + AR_GetFileName(color_palette) + "." + this->image_format << std::endl;
				else			stream << "out=" << file_path.substr(0,file_path.size()-4) + "." + this->image_format << std::endl;

				stream << ";image position relative to group position\n";
				stream << ";group outline and padding -> [x,y]+2\n";
			}
			stream << "image_total=" << im_num << std::endl;
			stream << "group_total=" << g_num << std::endl;
			stream << "----------------";
			this->tx_info.Write(stream.str());

			int surface = 0, max_width = 0, width_fin = 0, height_fin = 0;

			//calculate group sizes, total surface and sort if reguired
			AR_PrepareGroups(groups,surface,max_width);

			AR_Node node;

			//calculate width (final image should be as much w==h as possible)
			node.w = node.h = sqrt((float)surface);
			if(node.w<max_width) node.w = max_width;

			//expand height so it fits 100% sure, we will cut later using width_fin and height_fin
			node.h = 2048;

			//store pixel values in temporary matrix, using int so -1 can mark an outline pixel
			std::vector<std::vector<int> > matrix_one(node.h,std::vector<int>(node.w));		

			//create group matrix
			for(unsigned int i=0;i<groups.size();i++)
			{
				if(groups[i].images.empty()) continue;//default all in one group

				std::stringstream fpg;
				
				bool use_hires_pallete = false;
				std::string name = groups[i].name;
				std::size_t found = name.find("_hires");
				if(found!=std::string::npos && pal_hires)
				{
					use_hires_pallete = true;
					this->log->Write("Using hires pallete");
				}

				std::vector<image*> images = groups[i].images;//readibility

				fpg << "group=" << groups[i].name << std::endl;
				if(this->output_animation==AR_OCV_FILEPERGROUP)
				{
					if(pal_custom)	fpg << "out=" << file_path.substr(0,file_path.size()-4) + "_" + groups[i].name + "_" + AR_GetFileName(color_palette) + "." + this->image_format << std::endl;
					else			fpg << "out=" << file_path.substr(0,file_path.size()-4) + "_" + groups[i].name + "." + this->image_format << std::endl;
				}
				fpg << "image_total=" << images.size() << std::endl;//don't touch endl

				int width = groups[i].w;
				int height = groups[i].h;

				//store pixel values in temporary matrix
				std::vector<std::vector<int> > matrix(height,std::vector<int>(width));

				//posy and posy is the position where the top-left corner of the frame will be positioned in the group output
				int posx = 0, posy = 0;

				if(this->output_animation==AR_OCV_FILEPERSPEC)
				{
					//add 1 pixel for group outline, and 1 for gap/padding
					posx = posy = 2;					

					//-1 value marks an outline 
					for(int k=0;k<width;k++)
					{
						matrix[0][k] = -1;
						matrix[height-1][k] = -1;
					}

					for(int k=0;k<height;k++)
					{
						matrix[k][0] = -1;
						matrix[k][width-1] = -1;
					}
				}

				std::stringstream fpg2;
				fpg2 << ";name x y w h" << std::endl;
				fpg2 << ";--------------------------" << std::endl;

				for(unsigned int j=0;j<images.size();j++)//column
				{
					image *im = images[j];

					bool pixel_alpha = true;
					if(im->ar_type==SPEC_IMAGE || im->ar_type==SPEC_BACKTILE) pixel_alpha = false;
					
					for(int m=0;m<im->Size().y;m++)//row
						for(int n=0;n<im->Size().x;n++)//column							
						{
							int pixel = im->AR_GetPixels()[m*im->Size().x + n];

							//mark hires pallete pixels adding 1000 to the value
							if(use_hires_pallete) pixel += 1000;							

							//SPEC file can have images with different this->alpha settings
							//so we mark the this->alpha disabled/solid ones with a +10000 value in the output matrix
							if(!pixel_alpha) pixel += 10000;

							matrix[posy+m][posx+n] = pixel;
						}

						fpg2 << im->ar_name_o << " ";
						fpg2 << posx << " " << posy << " ";
						fpg2 << im->Size().x << " " << im->Size().y << std::endl;

						posx += im->Size().x + this->tilemap_padding;
				}

				if(this->output_animation==AR_OCV_FILEPERGROUP)
				{
					if(!AR_CreateImage(matrix,width,height,file_path.substr(0,file_path.size()-4) + "_" + groups[i].name,pal,pal_hires,pal_custom))
						return false;

					this->tx_info.Write(fpg.str());
					this->tx_info.Write(fpg2.str());
				}
				else if(this->output_animation==AR_OCV_FILEPERSPEC)
				{
					AR_Node *result = node.Insert(width,height);
					if(result)
					{
						for(int m=0;m<height;m++)//row
							for(int n=0;n<width;n++)//column
								matrix_one[result->y+m][result->x+n] = matrix[m][n];

						//final size of output image
						if(result->x + result->w > width_fin)	width_fin = result->x + result->w;
						if(result->y + result->h > height_fin)	height_fin = result->y + result->h;

						this->log->Write(groups[i].name);

						fpg << "group_position=" << result->x << " " << result->y;

						//frame positions are realtive from group position
						this->tx_info.Write(fpg.str());
						this->tx_info.Write(fpg2.str());
					}
					else
					{
						//doesn't fit
						this->log->Write("\nERROR - AR_ConvertSPEC() - doesn't fit \"" + groups[i].name + "\"\n");
						return false;
					}
				}
			}

			if(this->output_animation==AR_OCV_FILEPERSPEC)
			{
				if(!AR_CreateImage(matrix_one,width_fin,height_fin,file_path.substr(0,file_path.size()-4),pal,pal_hires,pal_custom))
					return false;
			}
		}
	}

	//delete images
	for(unsigned int i=0;i<groups.size();i++)
		for(unsigned int j=0;j<groups[i].images.size();j++)
			delete groups[i].images[j];

	//delete palettes
	if(pal)			delete pal;
	if(pal_hires)	delete pal_hires;
	if(pal_custom)	delete pal_custom;

	this->tx_info.Write("");

	return true;
}


palette* AR_SPEC::AR_GetPalette(std::string file_path)
{
	//get pallete (or tint from the tint folder to be precise)

	this->log->Write("Loading palette \"" + file_path + "\"...");

	jFILE fp(file_path.c_str(),"r");
	if(fp.open_failure())
	{
		this->log->Write("\nERROR - AR_GetPalette() - could not open \"" + file_path + "\"\n");
		return NULL;
	}

	//read file entries
	spec_directory dir(&fp);
	dir.FullyLoad(&fp);

	for(int i=0;i<dir.total;i++)
		if(dir.entries[i]->type==SPEC_PALETTE)
		{
			palette *pal = new palette(dir.entries[i],&fp);
			pal->ar_name = AR_GetFileName(file_path);
			return pal;
		}

		this->log->Write("\nERROR - AR_GetPalette() - could not find pallete in \"" + file_path + "\"\n");

		return NULL;
}

void AR_SPEC::AR_PrepareGroups(std::vector<AR_ImageGroup> &groups, int &surface, int &max_width)
{
	for(unsigned int i=0;i<groups.size();i++)
	{
		std::vector<image*> images = groups[i].images;//readibility

		//calculate output image size
		int width = 0;//sum of image widths + gap/padding
		for(unsigned int j=0;j<images.size();j++)
		{
			width += images[j]->Size().x;
			if(j<images.size()-1) width += this->tilemap_padding;//add gap/padding between two frames
		}

		int height = 0;//use max height
		for(unsigned int j=0;j<images.size();j++)
			if(images[j]->Size().y>height)
				height = images[j]->Size().y;

		if(this->output_animation==AR_OCV_FILEPERSPEC)
		{
			//add 1 pixel for group outline, and 1 for gap/padding
			groups[i].w = width + 4;
			groups[i].h = height + 4;
		}
		else
		{
			groups[i].w = width;
			groups[i].h = height;
		}


		surface += groups[i].w*groups[i].h;

		if(groups[i].w>max_width) max_width = groups[i].w;
	}

	//bubble sort based on surface area, max->min
	for(unsigned int i=0;i<groups.size()-1;i++)
		for(unsigned int j=i+1;j<groups.size();j++)
			if(groups[j].w*groups[j].h>groups[i].w*groups[i].h)
			{
				//swap
				AR_ImageGroup g = groups[i];
				groups[i] = groups[j];
				groups[j] = g;
			}
}

int AR_SPEC::AR_GetGroup(std::string name, std::vector<AR_ImageGroup> &groups)
{
	for(unsigned int i=0;i<groups.size();i++)
		if(groups[i].name==name)
			return i;

	return -1;//add new group
}

image* AR_SPEC::AR_GetImageByName(std::string name, std::vector<AR_ImageGroup> &groups)
{
	for(unsigned int i=0;i<groups.size();i++)
		for(unsigned int j=0;j<groups[i].images.size();j++)
			if(groups[i].images[j]->ar_name==name)
				return groups[i].images[j];

	return NULL;
}

//////////
////////// OpenCV
//////////

bool AR_SPEC::AR_CreateTile(image *im, std::string path, palette *pal, palette *pal_custom)
{
	Mat ocv;

	palette *pal_fin = pal;
	if(pal_custom)
	{
		//use preloaded pallete
		pal_fin = pal_custom;
		path += "_" + AR_GetFileName(color_palette);
	}
	
	bool pixel_alpha = true;
	if(im->ar_type==SPEC_IMAGE || im->ar_type==SPEC_BACKTILE) pixel_alpha = false;
	
	//only png supports this->alpha channel
	if(this->image_format=="png" && this->alpha==AR_OCV_COLORTOALPHA)
		ocv = cvCreateImage(cvSize(im->Size().x,im->Size().y),IPL_DEPTH_8U,4);
	else
		ocv = cvCreateImage(cvSize(im->Size().x,im->Size().y),IPL_DEPTH_8U,3);

	int k = 0;
	for(int i=0;i<ocv.rows;i++)
	{
		uchar *row = ocv.ptr<uchar>(i);

		for(int j=0;j<ocv.cols;j++)
		{
			k = i*im->Size().x+j;
			
			//OpenCV uses BGRA (not ARGB or RGBA)

			if(im->AR_GetPixels()[k]==0 && this->alpha==AR_OCV_NEWCOLOR && pixel_alpha)
			{
				//animated image, transparent pixel, change transparent pixel color enabled
				row[j*ocv.channels()+0] = this->alpha_b;
				row[j*ocv.channels()+1] = this->alpha_g;
				row[j*ocv.channels()+2] = this->alpha_r;
			}
			else
			{
				//keep original color
				row[j*ocv.channels()+0] = pal_fin->blue(im->AR_GetPixels()[k]);
				row[j*ocv.channels()+1] = pal_fin->green(im->AR_GetPixels()[k]);
				row[j*ocv.channels()+2] = pal_fin->red(im->AR_GetPixels()[k]);
			}
			
			//animated image, this->alpha channel enabled, png
			if(this->alpha==AR_OCV_COLORTOALPHA && this->image_format=="png")
			{
				if(im->AR_GetPixels()[k]==0 && pixel_alpha) row[j*ocv.channels()+3] = 0;//transparent
				else row[j*ocv.channels()+3] = 255;//solid
			}
		}
	}

	if(AR_SaveImage(ocv,path))
	{
		//original name in SPEC, output name, w, h
		std::stringstream stream;
		stream << im->ar_name_o << " ";
		stream << path + "." + this->image_format << " ";
		stream << im->Size().x << " " << im->Size().y;
		this->tx_info.Write(stream.str());

		return true;
	}
	
	return false;
}

bool AR_SPEC::AR_CreateImage(std::vector<std::vector<int> > &m, int w, int h, std::string name, palette *pal, palette *pal_hires, palette *pal_custom)
{
	//convert image to modern formats using OpenCV

	Mat ocv;

	palette *pal_fin = NULL;

	//only png supports this->alpha channel
	if(this->image_format=="png" && this->alpha==AR_OCV_COLORTOALPHA)
		ocv = cvCreateImage(cvSize(w,h),IPL_DEPTH_8U,4);
	else
		ocv = cvCreateImage(cvSize(w,h),IPL_DEPTH_8U,3);

	for(int i=0;i<ocv.rows;i++)
	{
		uchar *row = ocv.ptr<uchar>(i);

		for(int j=0;j<ocv.cols;j++)
		{
			//OpenCV uses BGRA (not ARGB or RGBA)

			int pixel = m[i][j];
			bool pixel_alpha = true;

			//solid pixels from backtiles or images are marked by adding 10000 to the value
			if(pixel>=10000)
			{
				pixel -= 10000;
				pixel_alpha = false;
			}

			if(pal_custom) pal_fin = pal_custom;//use preloaded pallete
			else
			{
				//pixel values over 1000 mark pixels from images using hires palette
				if(pixel>=1000)
				{
					if(pal_hires) pal_fin = pal_hires;
					else pal_fin = pal;
					pixel -= 1000;
				}
				else pal_fin = pal;
			}

			if(pixel==-1)
			{
				//-1 value marks colored outline
				row[j*ocv.channels()+0] = this->outline_b;//blue
				row[j*ocv.channels()+1] = this->outline_g;//green
				row[j*ocv.channels()+2] = this->outline_r;//red
			}			
			else if(pixel==0 && this->alpha==AR_OCV_NEWCOLOR && pixel_alpha)
			{
				//animated image, transparent pixel, change transparent pixel color enabled
				row[j*ocv.channels()+0] = this->alpha_b;
				row[j*ocv.channels()+1] = this->alpha_g;
				row[j*ocv.channels()+2] = this->alpha_r;
			}
			else
			{
				//keep original color
				row[j*ocv.channels()+0] = pal_fin->blue(pixel);
				row[j*ocv.channels()+1] = pal_fin->green(pixel);
				row[j*ocv.channels()+2] = pal_fin->red(pixel);
			}

			//animated image, this->alpha channel enabled, png
			if(this->alpha==AR_OCV_COLORTOALPHA && this->image_format=="png")
			{
				if(pixel==0 && pixel_alpha) row[j*ocv.channels()+3] = 0;//transparent
				else row[j*ocv.channels()+3] = 255;//solid
			}
		}
	}

	if(pal_custom) name += "_" + pal_custom->ar_name;

	return AR_SaveImage(ocv,name);
}

bool AR_SPEC::AR_SaveImage(Mat &ocv, std::string path)
{
	//set parameters and save image

	bool result = false;

	std::string path_fin = path + "." + this->image_format;

	if(this->image_format=="png")
	{
		vector<int> parameters;
		parameters.push_back(CV_IMWRITE_PNG_COMPRESSION);
		parameters.push_back(this->png_compression);

		result = imwrite(path_fin,ocv,parameters);
	}
	else if(this->image_format=="jpeg" || this->image_format=="jpg" | this->image_format=="jpe" || this->image_format=="jp2")
	{
		vector<int> parameters;
		parameters.push_back(CV_IMWRITE_JPEG_QUALITY);
		parameters.push_back(this->jpeg_quality);

		result = imwrite(path_fin,ocv,parameters);
	}
	else result = imwrite(path_fin,ocv);

	if(result)	this->log->Write("Saved to \"" + path_fin + "\"");
	else		this->log->Write("\nERROR - imwrite() failed \"" + path_fin + "\"\n");

	return result;
}