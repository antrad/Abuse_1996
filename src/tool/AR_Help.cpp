//
// Copyright (c) 2016 Antonio Radojkovic <antonior.software@gmail.com>
//
//   This is free software; you can redistribute it and/or
//   modify it under the terms of the Do What The Fuck You Want To
//   Public License, Version 2, as published by Sam Hocevar.
//

#include "AR_Help.h"

//////////
////////// Random
//////////

std::string AR_GetFileName(std::string path)
{
	std::size_t found = path.find_last_of("/\\");

	//no extension
	if(found!=std::string::npos) return path.substr(found+1,path.size()-(found+1)-4);
	else return "";
}

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

//////////
////////// Log
//////////

AR_Log::AR_Log()
{
	this->out_path = "log.txt";
	this->console = false;
};

AR_Log::AR_Log(std::string out_path, bool console)
{
	this->out_path = out_path;
	this->console = console;
};

void AR_Log::Write(std::string text, bool save)
{
	text += "\n";

	this->log << text;

	if(save)
	{
		std::ofstream fileout(this->out_path.c_str());
		if(fileout.is_open())
		{
			fileout << this->log.str();
			fileout.close();
		}
		else
		{
			std::string msg = "ERROR - Failed saving log at \"" + out_path + "\"";
			printf(msg.c_str());
		}
	}

	if(console) printf(text.c_str());
}

//////////
////////// Texture packing
//////////

AR_Node::AR_Node()
{
	this->x = y = w = h = 0;
	this->image = false;
}

AR_Node* AR_Node::Insert(int img_w, int img_h)
{
	//if we are not a leaf then
	if(!this->child.empty())
	{
		//try inserting into first child
		AR_Node *newNode = this->child[0].Insert(img_w,img_h);
		if(newNode!=NULL) return newNode;

		//no room in first child, insert into second
		return this->child[1].Insert(img_w,img_h);
	}
	else
	{
		//if there is already a image here, return
		if(this->image) return NULL;

		//if image doesn't fit, return
		if(this->w<img_w || this->h<img_h) return NULL;

		//if we're just right, accept
		if(this->w==img_w && this->h==img_h)
		{
			this->image = true;
			return this;
		}

		//otherwise split this node and create some children
		this->child.push_back(AR_Node());
		this->child.push_back(AR_Node());

		//decide which way to split
		int dw = this->w - img_w;
		int dh = this->h - img_h;

		if(dw>dh)
		{
			//vertical split
			//left
			this->child[0].x = this->x;
			this->child[0].y = this->y;
			this->child[0].w = img_w;
			this->child[0].h = this->h;
			//right
			this->child[1].x = this->x + img_w;
			this->child[1].y = this->y;
			this->child[1].w = this->w - img_w;
			this->child[1].h = this->h;
		}
		else
		{
			//horizontal split
			//up
			this->child[0].x = this->x;
			this->child[0].y = this->y;
			this->child[0].w = this->w;
			this->child[0].h = img_h;
			//down
			this->child[1].x = this->x;
			this->child[1].y = this->y + img_h;
			this->child[1].w = this->w;
			this->child[1].h = this->h - img_h;
		}

		//insert into first child we created
		return this->child[0].Insert(img_w,img_h);
	}
}