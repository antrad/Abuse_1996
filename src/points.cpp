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

#include "common.h"

#include "points.h"
#include "dev.h"

point_list::point_list(unsigned char how_many, unsigned char *Data)
{
  tot=how_many;
  if (tot)
  {
    data=(unsigned char *)malloc((int)how_many*2);
    memcpy(data,Data,(int)tot*2);
  } else data=NULL;
}

point_list::point_list(bFILE *fp)
{
  fp->read(&tot,1);
  if (tot)
  {
    data=(unsigned char *)malloc((int)tot*2);
    fp->read(data,(int)tot*2);

    int i;
    for (i=0; i<tot*2; i++)
      data[i]=data[i]*scale_mult/scale_div;

  } else data=NULL;
}

void point_list::save(bFILE *fp)
{
  fp->write(&tot,1);
  if (tot) fp->write(data,(int)tot*2);
}



