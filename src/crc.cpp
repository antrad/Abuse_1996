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

#include "crc.h"

uint16_t calc_crc(void *buf, size_t len)
{
    uint8_t *data = (uint8_t *)buf;
    uint8_t c1 = 0, c2 = 0;

    while (len--)
        c2 += (c1 += *data++);

    return (c2 << 8) | c1;
}



uint32_t crc_file(bFILE *fp)
{
  uint8_t crc1=0,crc2=0,crc3=0,crc4=0;

  int size=0x1000;
  uint8_t *buffer=(uint8_t *)malloc(size),*c;
  long l=fp->file_size();
  long cur_pos=fp->tell();
  fp->seek(0,0);
  while (l)
  {
    int nr=fp->read(buffer,size);
    if (nr==0) l=0;
    else
    {
      l-=nr;
      for (c=buffer; nr; nr--,c++)
      {
    crc1+=*c;
    crc2+=crc1;
    crc3+=crc2;
    crc4+=crc3;
      }
    }
  }
  fp->seek(cur_pos,0);
  free(buffer);
  return (crc1|(crc2<<8)|(crc3<<16)|(crc4<<24));
}
