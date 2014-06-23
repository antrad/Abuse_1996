/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __FAKELIB_HPP_
#define __FAKELIB_HPP_

#include <stdint.h>

class bFILE
{
  public :
  FILE *fp;
  bFILE(FILE *FP) { fp=FP; }
  bFILE(char *fn, char *mode) { fp=fopen(fn,mode); }
  long file_size() { long cur=ftell(fp),ret; fseek(fp,0,2); ret=ftell(fp);
             fseek(fp,cur,0); return ret; }
  int read(void *buf, int count) { return fread(buf,count,1,fp); }
  int write(void *buf, int count) { return fwrite(buf,count,1,fp); }
  int write_byte(uchar x) { return fputc(x,fp); }
  int open_failure() { return fp==NULL; }
  ~bFILE() { if (fp) fclose(fp); }
} ;

#define jFILE bFILE

bFILE *open_file(char *name, char *perm) { return new bFILE(fopen(name,perm)); }
#define dprintf printf
void dgets(char *s, int x)
{ fgets(s,x,stdin);
  if (strlen(s)>0) s[strlen(s)-1]=0;
}

#endif
