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

#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

#include "specs.h"
#include "nfserver.h"
#include "dprint.h"
#include "crc.h"
#include "cache.h"

class nfs_file : public bFILE
{
  jFILE *local;
  int nfs_fd;
  int offset;
  public :
  nfs_file(char *filename, char *mode);
  virtual int open_failure();
  virtual int unbuffered_read(void *buf, size_t count);       // returns number of bytes read
  int new_read(void *buf, size_t count);       // returns number of bytes read
  virtual int unbuffered_write(void *buf, size_t count);      // returns number of bytes written
  virtual int unbuffered_seek(int32_t offset, int whence);  // whence=SEEK_SET, SEEK_CUR, SEEK_END, ret=0=success
  virtual int unbuffered_tell();
  virtual int file_size();
  virtual ~nfs_file();
} ;

bFILE *open_nfs_file(char *filename,char *mode)
{
  return new nfs_file(filename,mode);
}


static out_socket *nfs_server=NULL;
void connect_to_nfs_server(char *name, int port)
{
  nfs_server=create_out_socket(name,port);
  if (!nfs_server)
  {
    fprintf(stderr,"%s\n",last_sock_err);
    exit(0);
  } else
  {
    set_file_opener(open_nfs_file);   // from now on all files wll go through NFS creator
  }
}

static void nfs_disconnect()
{
  if (nfs_server)
  {
    delete nfs_server;
    nfs_server=NULL;
    set_file_opener(NULL);    // use local means for opening files
  }
}


nfs_file::nfs_file(char *filename, char *mode)
{
  int local_only=0;
  for (char *s=mode; *s; s++)    // check to see if writeable file, if so don't go through nfs
    if (*s=='w' || *s=='W' || *s=='a' || *s=='A')
      local_only=1;
  if (local_only)
  {
    local=new jFILE(filename,mode);
    nfs_fd=-1;
  }
  else
  {
    local=NULL;
    nfs_fd=-1;
    if (nfs_server)
    {
      packet pk;
      int do_open=0;
      jFILE *local_test=new jFILE(filename,mode);
      if (local_test->open_failure())
      {
    delete local_test;
    local_test=NULL;
    pk.write_uint8(NFS_OPEN);
      }
      else
      {
    pk.write_uint8(NFS_CRC_OPEN);
    int fail;
    uint32_t crc=crc_manager.get_crc(crc_manager.get_filenumber(filename),fail); // skip crc calc if we can
    if (fail) crc=crc_file(local_test);
    pk.write_uint32(crc);
      }

      pk.write_uint8(strlen(filename)+1);
      pk.write((uint8_t *)filename,strlen(filename)+1);
      pk.write_uint8(strlen(mode)+1);
      pk.write((uint8_t *)mode,strlen(mode)+1);
      dprintf("try open %s,%s\n",filename,mode);
      offset=0;
      if (!nfs_server->send(pk))
        nfs_disconnect();
      else
      {
    if (!nfs_server->get(pk)) nfs_disconnect();
    else
    {
      int32_t fd;
      if (pk.read((uint8_t *)&fd,4)!=4)
        nfs_disconnect();
      else
      {
        fd=lltl(fd);
        nfs_fd=fd;
        if (local_test && fd==-2) // confirmed that CRCs match, use local file
        { local=local_test;    local_test=NULL; }

      }
    }
      }
      if (local_test)
        delete local_test;
    }
  }
}


int nfs_file::open_failure() { return !local && nfs_fd==-1; }

int nfs_file::unbuffered_read(void *buf, size_t count)      // returns number of bytes read
{
  if (local)
    return local->read(buf,count);
  else
  {
    int ret=new_read(buf,count);
    void *comp=malloc(count);
/*    ::read(check_fd,comp,count);
    if (memcmp(comp,buf,count))
    {
      printf("bad read!\n");
    }
    free(comp); */
    return ret;
  }
}

int nfs_file::new_read(void *buf, size_t count)      // returns number of bytes read
{
  if (local)
    return local->read(buf,count);
  else
  {
    packet pk;
    pk.write_uint8(NFS_READ);
    pk.write_uint32(nfs_fd);
    pk.write_uint32(count);
    dprintf("try read %d,%d\n",nfs_fd,count);
    if (!nfs_server->send(pk))
    {
      nfs_disconnect();
      return 0;
    }
    else
    {

      int fail=0;
      int rtotal=0;
      uint16_t size=1;
      while (count>0 && !fail && size)
      {
    if (!nfs_server->get(pk)) fail=1;
    else
    {
      if (pk.read((uint8_t *)&size,2)!=2) fail=1;
      else
      {
        size=lstl(size);
        printf("read %d bytes\n",size);
        if (size)
        {
          int need_size=size>count ? count : size;

          if (pk.read((uint8_t *)buf,need_size)!=need_size) fail=1;
          else
          {
        count-=need_size;
        rtotal+=need_size;
        buf=(void *)(((char *)buf)+need_size);
        offset+=need_size;
        if (need_size<size)    // see if there are any leftovers to buffer
          fprintf(stderr,"Server sent to much\n");
          }
          if (need_size<2048) count=0;
        }
      }
    }
      }
      if (fail)
      {
    dprintf("nfs read failed\n");
        nfs_disconnect();
      }
      return rtotal;
    }
  }
}


int nfs_file::unbuffered_write(void *buf, size_t count)      // returns number of bytes written
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


int nfs_file::unbuffered_seek(int32_t off, int whence) // whence=SEEK_SET, SEEK_CUR, SEEK_END, ret=0=success
{
  if (local)
    return local->seek(off,whence);
  else
  {
    packet pk;
    pk.write_uint8(NFS_SEEK);
    pk.write_uint32(nfs_fd);

    pk.write_uint32(off);
    pk.write_uint32(whence);
    dprintf("seek %d %d %d\n",nfs_fd,off,whence);
    if (!nfs_server->send(pk))
    {
      dprintf("disconnected on seek\n");
      nfs_disconnect();
      return 0;
    }
  }
  return 0;
}

int nfs_file::unbuffered_tell()
{
  if (local)
    return local->tell();
  else if (nfs_server)
  {
    packet pk;
    pk.write_uint8(NFS_TELL);
    pk.write_uint32(nfs_fd);
    if (!nfs_server->send(pk))
    {
      nfs_disconnect();
      return 0;
    } else
    {
      if (!nfs_server->get(pk))
      {
    nfs_disconnect();
    return 0;
      } else
      {
    int32_t off;
    if (pk.read((uint8_t *)&off,4)!=4)
    {
      dprintf("Disconnected on tell()\n");
      nfs_disconnect();
    } else return lltl(off);

      }
    }
  }
  return 0;
}


int nfs_file::file_size()
{
  if (local)
    return local->file_size();
  else if (nfs_server)
  {
    packet pk;
    pk.write_uint8(NFS_FILESIZE);
    pk.write_uint32(nfs_fd);
    if (!nfs_server->send(pk))
    {
      nfs_disconnect();
      return 0;
    } else
    {
      if (!nfs_server->get(pk))
      {
    nfs_disconnect();
    return 0;
      } else
      {
    int32_t size;
    if (pk.read((uint8_t *)&size,4)!=4)
    {
      dprintf("disconnected on filesize\n");
      nfs_disconnect();
    }
    else return lltl(size);
      }
    }
  }
  return 0;
}

nfs_file::~nfs_file()
{
  flush_writes();
  if (local) delete local;
  else if (nfs_server && !open_failure())
  {
    packet pk;
    pk.write_uint8(NFS_CLOSE);
    pk.write_uint32(nfs_fd);
    dprintf("close %d\n",nfs_fd);
    if (!nfs_server->send(pk))
    {
      dprintf("disconnected on close\n");
      nfs_disconnect();
    }
  }
}




