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

#include <time.h>
#include <stdio.h>

#include "jrand.h"

unsigned short rtable[RAND_TABLE_SIZE];
unsigned short rand_on=0;

void jrand_init()
{
  // make sure random table is always the same.
  unsigned long rseed=('F'<<24)|('U'<<16)|('C'<<8)|'K';
  int i;
  unsigned short *tp=rtable;
  for (i=0; i<RAND_TABLE_SIZE; i++,tp++)
  {
    rseed=rseed*0x41c64e6d+12345;
    *tp=(rseed>>16)&0xffff;
  }

  time_t t=time(NULL);         // get an original random seed now.
  rand_on=t%RAND_TABLE_SIZE;
}

