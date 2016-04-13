//
// Abuse Tool - package manager for Abuse format
//
// Copyright: (c) 2011 Sam Hocevar <sam@hocevar.net>
// Copyright: (c) 2016 Antonio Radojkovic <antonior.software@gmail.com>
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the Do What The Fuck You Want To
//   Public License, Version 2, as published by Sam Hocevar. See
//   http://sam.zoy.org/projects/COPYING.WTFPL for more details.
//

#if defined HAVE_CONFIG_H
#   include "config.h"
#endif

#include <cstring>
#include <cstdio>

#include "common.h"
#include "specs.h"
#include "image.h"
#include "pcxread.h"
#include "crc.h"

#include "AR_SPEC.h"

enum
{
	CMD_INVALID,
	CMD_LIST,
	CMD_GET,
	CMD_MOVE,
	CMD_DEL,
	CMD_PUT,
	CMD_RENAME,
	CMD_TYPE,
	CMD_GETPCX,
	CMD_PUTPCX
};

//art/chars/ammo.spe	- some images have "/" in their name, it will fail to save individual images,
//						because it will think it is a folder, it's OK if everything is saved in one file

AR_Log ar_log("abuse-tool/log.txt",true);

void Usage();
int abuse_tool(int argc, char *argv[]);

int main(int argc, char *argv[])
{
	int result = 0;

	if(argc==1)
	{
		AR_SPEC ar_spec;
		ar_spec.log = &ar_log;

		result = ar_spec.AR_ParseConfig("abuse-tool/extract.txt");

		if(result==EXIT_SUCCESS) ar_log.Write("\nAR_ParseConfig()..... OK");
		else ar_log.Write("\nAR_ParseConfig()..... FAIL");

		ar_spec.tx_info.Write("",true);
	}
	else result = abuse_tool(argc,argv);

	ar_log.Write("",true);
	
	return result;
}

void Usage()
{
	printf(
		"\n"
		"Abuse-tool is a low-level tool to edit Abuse SPEC (.spe) files\n"
		"and to extract PSX image files contained in them to modern image formats\n"
		"as individual images, tilemaps or a texture atlas.\n"
		"\n\n"
		"Usage 1: abuse-tool <spec_file> <command> [args...]\n"
		"\n"
		"Commands:\n"
		"   list                        list the contents of a SPEC file\n"
		"   get     <id>                dump entry <id> to stdout\n"
		"   getpcx  <id>                dump PCX image <id> to \"..\\abuse-tool\"\n"
		"   put     <id> <type> <name>  insert file <name> of type <type> at position <id>\n"
		"   putpcx  <id> <type> <name>  insert PCX image <name> of type <type> at position <id>\n"
		"   rename  <id> <name>         rename entry <id> to <name>\n"
		"   type    <id> <type>         set entry <id> type to <type>\n"
		"   move    <id1> <id2>         move entry <id1> to position <id2>\n"
		"   del     <id>                delete entry <id>\n"
		"\n"
		"See the abuse-tool(6) manual page for more information.\n"
		"\n"
		"Usage 2: abuse-tool\n"
		"\n"
		"   To extract the images to modern image formats the program reads\n"
		"   the settings and list of files located in \"..\\abuse-tool\\extract.txt\"\n"
		"   when being launched. More info can be found there.\n"
		"   Extracted images will be stored at the location of the files.\n"		
		"\n"
		);
}

int abuse_tool(int argc, char *argv[])
{
	if(argc<3)
	{
		Usage();
		return EXIT_FAILURE;
	}

	ar_log.Write("\n\nUsage 1: abuse-tool <spec_file> <command> [args...]");
	ar_log.Write("---------------------------------------------------\n");

	int cmd = !strcmp(argv[2], "list") ? CMD_LIST
		: !strcmp(argv[2], "get") ? CMD_GET
		: !strcmp(argv[2], "del") ? CMD_DEL
		: !strcmp(argv[2], "put") ? CMD_PUT
		: !strcmp(argv[2], "move") ? CMD_MOVE
		: !strcmp(argv[2], "rename") ? CMD_RENAME
		: !strcmp(argv[2], "type") ? CMD_TYPE
		: !strcmp(argv[2], "getpcx") ? CMD_GETPCX
		: !strcmp(argv[2], "putpcx") ? CMD_PUTPCX
		: CMD_INVALID;

	if (cmd == CMD_INVALID)
	{
		char buffer[512];
		sprintf("\nERROR - abuse-tool: unknown command `%s'\n", argv[2]);
		ar_log.Write(buffer);

		return EXIT_FAILURE;
	}

	/* Check argument count and file access mode */
	char const *mode = "rwb";
	int minargc = 3;

	switch (cmd)
	{
	case CMD_LIST:		mode = "rb";				break;// Read-only access
	case CMD_GET:		minargc = 4;mode = "rb";	break;// Read-only access
	case CMD_PUT:		minargc = 6;				break;
	case CMD_MOVE:		minargc = 5;				break;
	case CMD_RENAME:	minargc = 5;				break;
	case CMD_TYPE:		minargc = 5;				break;
	case CMD_DEL:		minargc = 4;				break;
	case CMD_GETPCX:	minargc = 4;mode = "rb";	break;// Read-only access
	case CMD_PUTPCX:	minargc = 6;				break;
	}

	if(argc < minargc)
	{
		char buffer[512];
		sprintf("\nERROR - abuse-tool: too few arguments for command `%s'\n", argv[2]);
		ar_log.Write(buffer);

		return EXIT_FAILURE;
	}

	/* Open the SPEC file */
	char tmpfile[4096];
	char const *file = argv[1];
	snprintf(tmpfile, 4096, "%s.tmp", file);

	//AR crashes if the file path is too long (desktop) ???
	//or when the fle name is wrong
	jFILE fp(file, mode);
	if(fp.open_failure())
	{
		char buffer[512];
		sprintf("\nERROR - abuse-tool: could not open %s\n", file);
		ar_log.Write(buffer);

		return EXIT_FAILURE;
	}

	std::stringstream file_title;
	file_title << "\nReading file \"" << file << "\":\n";
	ar_log.Write(file_title.str());

	spec_directory dir(&fp);

	/* Now really execute commands */
	if (cmd == CMD_LIST)
	{
		ar_log.Write("   id  type    bytes   crc  name & information");
		ar_log.Write(" ----  ----  -------  ----  ----------------------------\n");

		dir.FullyLoad(&fp);

		for (int i = 0; i < dir.total; i++)
		{
			spec_entry *se = dir.entries[i];

			std::stringstream row;

			/* Print basic information */
			char buffer[512];
			sprintf(buffer, "% 5i   % 3i % 8i  %04x  %s", i, se->type, (int)se->size, calc_crc(se->data, se->size), se->name);
			row << buffer;

			/* Is there anything special to say? */
			switch (se->type)
			{
			case SPEC_IMAGE:
			case SPEC_FORETILE:
			case SPEC_BACKTILE:
			case SPEC_CHARACTER:
			case SPEC_CHARACTER2:
				{
					image *im = new image(&fp, se);

					char buffer[512];
					sprintf(buffer, " \t# %i x %i pixels", im->Size().x, im->Size().y);
					row << " " << buffer;

					delete im;
					break;
				}
			case SPEC_PALETTE:
				{
					palette *pal = new palette(se, &fp);

					char buffer[512];
					sprintf(buffer, " \t# %i colors", pal->pal_size());
					row << " " << buffer;

					delete pal;
					break;
				}
#if 0
			default:
				{
					/* Try to print a representation of the item */
					int has_binary = 0;
					for (int i = 0; i < Min(20, (int)se->size); i++)
					{
						uint8_t ch = ((uint8_t *)se->data)[i];
						if (ch < 0x20 || ch >= 0x7f)
							has_binary++;
					}
					if (has_binary <= 2 && se->size > 5)
						has_binary = 0;

					printf(" \t# ");
					if (!has_binary)
						putchar('\"');

					size_t max = Min(has_binary ? 15 : 30, (int)se->size);
					for (size_t i = 0; i < max; i++)
					{
						uint8_t ch = ((uint8_t *)se->data)[i];
						if (has_binary)
							printf("%02x ", ch);
						else if (ch && (ch < 0x20 || ch >= 0x7f))
							printf("\\x%02x", ch);
						else if (ch)
							putchar(ch);
						else
							printf("\\0");
					}
					if (se->size > max)
						printf("...");
					else if (!has_binary)
						printf("\"");
					break;
				}
#endif
			}

			/* Finish line */
			ar_log.Write(row.str());
		}

		return EXIT_SUCCESS;
	}
	else if (cmd == CMD_GET)
	{
		int id = atoi(argv[3]);

		if (id < 0 || id >= dir.total)
		{
			char buffer[512];
			sprintf(buffer,"\nERROR - abuse-tool: id %i not found in %s\n", id, file);
			ar_log.Write(buffer);

			return EXIT_FAILURE;
		}

		spec_entry *se = dir.entries[id];
		fp.seek(se->offset, SEEK_SET);

		for (size_t todo = se->size; todo > 0; )
		{
			uint8_t buf[1024];
			int step = Min((int)todo, 1024);
			fp.read(buf, step);
			fwrite(buf, step, 1, stdout);
			todo -= step;
		}
		return EXIT_SUCCESS;
	}
	else if (cmd == CMD_GETPCX)
	{
		palette *pal;
		int imgid = atoi(argv[3]);
		int palid = argc > 4 ? atoi(argv[4]) : -1;

		for (int i = 0; palid == -1 && i < dir.total; i++)
			if (dir.entries[i]->type == SPEC_PALETTE)
				palid = i;

		if (palid == -1)
			pal = new palette(256);
		else
			pal = new palette(dir.entries[palid], &fp);

		image *im = new image(&fp, dir.entries[imgid]);

		//AR save locally and name it "file path+internal name" and add ".pcx" if it doesn' have it already
		std::stringstream path;
		path << "abuse-tool/" << AR_GetFileName(file) << "_" << dir.entries[imgid]->name;
		std::size_t found = path.str().find(".pcx");
		if(found==std::string::npos) path << ".pcx";

		write_PCX(im, pal, path.str().c_str());

		delete im;
		delete pal;
		return EXIT_SUCCESS;
	}
	else if (cmd == CMD_MOVE)
	{
		int src = atoi(argv[3]);
		int dst = atoi(argv[4]);

		if (src < 0 || dst < 0 || src >= dir.total || dst >= dir.total)
		{
			char buffer[512];
			sprintf(buffer,"\nERROR - abuse-tool: ids %i/%i out of range\n", src, dst);
			ar_log.Write(buffer);

			return EXIT_FAILURE;
		}

		dir.FullyLoad(&fp);

		spec_entry *tmp = dir.entries[src];
		for (int d = src < dst ? 1 : -1; src != dst; src += d)
			dir.entries[src] = dir.entries[src + d];
		dir.entries[dst] = tmp;
	}
	else if (cmd == CMD_RENAME || cmd == CMD_TYPE)
	{
		int id = atoi(argv[3]);

		if (id < 0 || id >= dir.total)
		{
			char buffer[512];
			sprintf(buffer,"\nERROR - abuse-tool: id %i out of range\n", id);
			ar_log.Write(buffer);

			return EXIT_FAILURE;
		}

		dir.FullyLoad(&fp);
		if (cmd == CMD_RENAME)
			dir.entries[id]->name = argv[4];
		else
			dir.entries[id]->type = (uint8_t)atoi(argv[4]);
	}
	else if (cmd == CMD_DEL)
	{
		int id = atoi(argv[3]);

		if (id < 0 || id >= dir.total)
		{
			char buffer[512];
			sprintf(buffer,"\nERROR - abuse-tool: id %i out of range\n", id);
			ar_log.Write(buffer);

			return EXIT_FAILURE;
		}

		dir.total--;
		for (int i = id; i < dir.total; i++)
			dir.entries[i] = dir.entries[i + 1];

		dir.FullyLoad(&fp);
	}
	else if (cmd == CMD_PUT || cmd == CMD_PUTPCX)
	{
		int id = atoi(argv[3]);
		uint8_t type = atoi(argv[4]);

		if (id == -1)
			id = dir.total;

		if (id < 0 || id > dir.total)
		{
			char buffer[512];
			sprintf(buffer,"\nERROR - abuse-tool: id %i out of range\n", id);
			ar_log.Write(buffer);

			return EXIT_FAILURE;
		}

		dir.FullyLoad(&fp);
		dir.total++;
		dir.entries = (spec_entry **)realloc(dir.entries,
			dir.total * sizeof(spec_entry *));
		for (int i = dir.total - 1; i-- > id; )
			dir.entries[i + 1] = dir.entries[i];

		char *name = strrchr(argv[5], '/');
		if (!name)
			name = argv[5];

		uint8_t *data;
		size_t len;

		if (cmd == CMD_PUT)
		{
			jFILE fp2(argv[5], "rb");
			if (fp2.open_failure())
			{
				char buffer[512];
				sprintf(buffer,"\nERROR - abuse-tool: cannot open %s\n", argv[5]);
				ar_log.Write(buffer);

				return EXIT_FAILURE;
			}
			len = fp2.file_size();
			data = (uint8_t *)malloc(len);
			fp2.read(data, len);
		}
		else
		{
			palette *pal = NULL;
			image *im = read_PCX(argv[5], pal);
			if (!im)
			{
				char buffer[512];
				sprintf(buffer,"\nERROR - abuse-tool: cannot open %s\n", argv[5]);
				ar_log.Write(buffer);

				return EXIT_FAILURE;
			}
			ivec2 size = im->Size();
			len = 2 * sizeof(uint16_t) + size.x * size.y;
			data = (uint8_t *)malloc(len);
			uint16_t x = lltl((uint16_t)size.x);
			uint16_t y = lltl((uint16_t)size.y);
			memcpy(data, &x, sizeof(x));
			memcpy(data + 2, &y, sizeof(y));
			memcpy(data + 4, im->scan_line(0), size.x * size.y);
		}
		dir.entries[id] = new spec_entry(type, name, NULL, len, 0);
		dir.entries[id]->data = data;
	}
	else
	{
		/* Not implemented yet */
		return EXIT_FAILURE;
	}

	/* If we get here, we need to write the directory back */
	dir.calc_offsets();
	fp.seek(0, SEEK_SET); // FIXME: create a new file
	dir.write(&fp);
	for (int i = 0; i < dir.total; i++)
		fp.write(dir.entries[i]->data, dir.entries[i]->size);

	return EXIT_SUCCESS;
}