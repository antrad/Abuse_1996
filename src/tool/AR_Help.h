//
// Copyright (c) 2016 Antonio Radojkovic <antonior.software@gmail.com>
//
//   This is free software; you can redistribute it and/or
//   modify it under the terms of the Do What The Fuck You Want To
//   Public License, Version 2, as published by Sam Hocevar.
//

#pragma once

#include <cstring>
#include <cstdio>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

//////////
////////// Random
//////////

std::string	AR_GetFileName	(std::string path);

int			AR_ToInt		(std::string value);
bool		AR_ToBool		(std::string value);

bool		AR_GetAttr		(std::string line, std::string &attr, std::string &value);

//////////
////////// Log
//////////

class AR_Log
{
private:
	std::stringstream log;//warning - can't copy a stringstream

public:
	std::string out_path;
	bool console;

	AR_Log();
	AR_Log(std::string out_path, bool console = false);

	void Write(std::string text, bool save = false);
};

//////////
////////// Texture packing
//////////

class AR_Node
{
public:
	std::vector<AR_Node> child;	//child nodes
	int x, y, w, h;				//position and node size
	bool image;					//image stored in the node	

	AR_Node();

	AR_Node* Insert(int img_w, int img_h);
};