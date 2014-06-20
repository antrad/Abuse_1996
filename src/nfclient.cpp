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

#if (defined(__MACH__) || !defined(__APPLE__))
#   include <sys/types.h>
#endif
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>

#include "common.h"

#include "netface.h"

#include "specs.h"
#include "nfserver.h"
#include "dprint.h"
#include "crc.h"
#include "cache.h"

#include "gserver.h"

#if !defined __CELLOS_LV2__

void remove_client(int client_number) { ; }

CrcManager *net_crcs = NULL;
extern net_protocol *prot;

class nfs_file : public bFILE
{
  jFILE *local;
  int nfs_fd;
  int offset;
  public :
  nfs_file(char const *filename, char const *mode);
  virtual int open_failure();
  virtual int unbuffered_read(void *buf, size_t count);       // returns number of bytes read
  int new_read(void *buf, size_t count);       // returns number of bytes read
  virtual int unbuffered_write(void const *buf, size_t count);      // returns number of bytes written
  virtual int unbuffered_seek(long offset, int whence);  // whence=SEEK_SET, SEEK_CUR, SEEK_END, ret=0=success
  virtual int unbuffered_tell();
  virtual int file_size();
  virtual ~nfs_file();
} ;

bFILE *open_nfs_file(char const *filename, char const *mode)
{
  return new nfs_file(filename,mode);
}


nfs_file::nfs_file(char const *filename, char const *mode)
{
  local=NULL;
  nfs_fd=-1;

  int local_only=0;
  char const *s=mode;
  for (; *s; s++)    // check to see if writeable file, if so don't go through nfs
    if (*s=='w' || *s=='W' || *s=='a' || *s=='A')
      local_only=1;

  char name[256], *c;
  char const *f = filename;
  c = name;
  while (*f) { *c=*(f++); *c=toupper(*c); c++; } *c=0;
  if (strstr(name,"REGISTER"))
    local_only=1;

  if (net_crcs && !local_only)
  {
    int fail1,fail2,fail3=0;
    char const *local_filename = filename;
    if (filename[0]=='/' && filename[1]=='/')
    { local_filename+=2;
      while (*local_filename && *local_filename!='/') local_filename++;
      local_filename++;
    }

    int remote_file_num=net_crcs->get_filenumber(local_filename);
    uint32_t remote_crc=net_crcs->get_crc(remote_file_num,fail2);
    if (!fail2)
    {
      int local_file_num=crc_manager.get_filenumber(local_filename);
      uint32_t local_crc=crc_manager.get_crc(local_file_num,fail1);
      if (fail1)
      {
    bFILE *fp=new jFILE(local_filename,"rb");
    if (!fp->open_failure())
    {
      local_crc=crc_file(fp);
      crc_manager.set_crc(local_file_num,local_crc);
    } else fail3=1;
    delete fp;
      }

      if (!fail3)
      {
    if (local_crc==remote_crc)
          local_only=1;
      }
    }
  }


  if (local_only)
  {
    local=new jFILE(filename,mode);
    if (local->open_failure()) { delete local; local=NULL; }
  }
  else
  {


    char nm[256];
    strcpy(nm,filename);
    nfs_fd=NF_open_file(nm,mode);
    if (nfs_fd==-2)
    {
      local=new jFILE(nm,mode);
      if (local->open_failure()) { delete local; local=NULL; }
      nfs_fd=-1;
    }
  }
}


int nfs_file::open_failure()
{
  if (local==NULL && nfs_fd<0) return 1;
  else return 0;
}


int nfs_file::unbuffered_read(void *buf, size_t count)      // returns number of bytes read
{
  if (local)
    return local->read(buf,count);
  else if (nfs_fd>=0)
  {
    long a=NF_read(nfs_fd,buf,count);
    if (a>(long)count)
    {
      fprintf(stderr,"ooch read too much\n");
    }
    return a;
  }
  else return 0;
}

int nfs_file::unbuffered_write(void const *buf, size_t count)      // returns number of bytes written
{
  if (local)
    return local->write(buf,count);
  else
  {
    fprintf(stderr,"write to nfs file not allowed for now!\n");
    exit(0);
  }
  return 0;
}


int nfs_file::unbuffered_seek(long off, int whence) // whence=SEEK_SET, SEEK_CUR, SEEK_END, ret=0=success
{
  if (local)
    return local->seek(off,whence);
  else if (nfs_fd>=0)
  {
    if (whence!=SEEK_SET)
      fprintf(stderr,"JC's a fork\n");
    else
      return NF_seek(nfs_fd,off);
  }
  return 0;
}

int nfs_file::unbuffered_tell()
{
  if (local)          return local->tell();
  else if (nfs_fd>=0) return NF_tell(nfs_fd);
  else                return 0;
}


int nfs_file::file_size()
{
  if (local)          return local->file_size();
  else if (nfs_fd>=0) return NF_filelength(nfs_fd);
  else                return 0;
}

nfs_file::~nfs_file()
{
  flush_writes();
  if (local)          delete local;
  else if (nfs_fd>=0) NF_close(nfs_fd);
}

int set_file_server(net_address *addr)
{
  if (NF_set_file_server(addr))
  {
    if (net_crcs)
    {
      net_crcs->clean_up();
      delete net_crcs;
    }

    net_crcs=new CrcManager();
    if (!net_crcs->load_crc_file(NET_CRC_FILENAME))
    {
      delete net_crcs;
      net_crcs=NULL;
      return 0;
    }
    return 1;
  }
  return 0;
}


int set_file_server(char const *name)
{
  if (prot)
  {
    net_address *addr=prot->get_node_address(name,DEFAULT_COMM_PORT,0);
    if (!addr) { dprintf("\nUnable to locate server\n"); return 0; }
    if (!set_file_server(addr))
    {
      delete addr;
      return 0;
    } else return 1;
  } else return 0;
}

#endif

