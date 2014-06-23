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

#include "common.h"

#include "include.h"
#include "ctype.h"

void write_include(image *im, palette *pal, char *filename, char *name)
{
  char tmp_name[200];
  strcpy(tmp_name,name);
  unsigned int j;
  int append=0,i;
  for (j=0; j<strlen(name); j++)
    if (toupper(tmp_name[j])<'A' || toupper(tmp_name[j])>'Z')
      tmp_name[j]='_';

  FILE *fp=fopen(filename,"rb");  // see if the file already exists
  if (fp)
  {
    fclose(fp);
    fp=fopen(filename,"ab");  // if so, append to the end and don't write the palette
    append=1;
  }
  else fp=fopen(filename,"wb");

  if (fp)
  {
    fprintf(fp,"/* File produced by Satan Paint (c) 1994 Jonathan Clark */\n\n");
    if (!append)
    {
      fprintf(fp,"unsigned char %s_palette[256*3] = {\n    ",tmp_name);
      unsigned char *p=(unsigned char *)pal->addr();
      for (i=0; i<768; i++,p++)
      {
    fprintf(fp,"%d",(int)*p);
    if (i==767)
        fprintf(fp,"};\n\n");
    else
        if (i%15==14)
    fprintf(fp,",\n    ");
        else fprintf(fp,", ");
      }
    }
    ivec2 size = im->Size();
    fprintf(fp,"unsigned char %s[%d*%d]={\n    ",tmp_name, size.x, size.y);
    int x,y,max=size.x*size.y-1;
    for (y=0,i=0; y<size.y; y++)
      for (x=0; x<size.x; x++,i++)
      {
        fprintf(fp,"%d",(int)im->Pixel(ivec2(x,y)));
        if (i==max)
          fprintf(fp,"};\n\n");
        else
          if (i%15==14)
            fprintf(fp,",\n    ");
          else fprintf(fp,", ");
      }
    fclose(fp);
  }
}

