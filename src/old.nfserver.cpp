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

#include "jnet.h"
#include "specs.h"
#include "nfserver.h"
#include "dprint.h"
#include "timing.h"
#include "cache.h"
#include "crc.h"

nfs_server *file_server=NULL;

class nfs_server_client_node
{
  public :
  out_socket *nd;
  nfs_server_client_node *next;
  bFILE **file_list;
  int file_list_size;


  nfs_server_client_node(out_socket *descriptor, nfs_server_client_node *Next)
  { nd=descriptor; next=Next;
    file_list_size=0;
    file_list=NULL;
  }
  int add_file(bFILE *fp);  // returns id for jFILE
  bFILE *get_file(int id);
  int delete_file(int id);  // returns 1=success

  ~nfs_server_client_node();

} ;

nfs_server_client_node::~nfs_server_client_node()
{
  delete nd;
  for (int i=0; i<file_list_size; i++)
    if (file_list[i])
     delete file_list[i];
  if (file_list)
    free(file_list);
}

int nfs_server_client_node::add_file(bFILE *fp)  // returns id for bFILE
{
  for (int i=0; i<file_list_size; i++)   // search for a free spot
  {
    if (!file_list[i])
    {
      file_list[i]=fp;
      return i;
    }
  }
  // we need to enlarge the file_list
  file_list_size++;
  file_list=(bFILE **)realloc(file_list,sizeof(bFILE *)*file_list_size);
  file_list[file_list_size-1]=fp;
  return file_list_size-1;
}

bFILE *nfs_server_client_node::get_file(int id)
{
  if (id<file_list_size)
    return file_list[id];
  else return NULL;
}

int nfs_server_client_node::delete_file(int id)
{
  if (id<file_list_size && file_list[id])
  {
    delete file_list[id];
    file_list[id]=NULL;
    return 1;
  }
  return 0;
}

char *squash_path(char *path, char *buffer)
{
  strcpy(buffer,path);
  return buffer;
}

int nfs_server::process_packet(packet &pk, nfs_server_client_node *c)
{
  uint8_t cmd;
  if (pk.read(&cmd,1)!=1)
  {
    dprintf("Could not read command from nfs packet\n");
    return 0;
  }
  dprintf("cmd : %d\n",cmd);
  switch (cmd)
  {
    case NFS_CLOSE_CONNECTION :
    { return 0; } break;
    case NFS_CRC_OPEN :
    {
      uint8_t fn_len;
      char fn[255],newfn[255],perm[255];
      uint32_t crc;
      if (pk.read((uint8_t *)&crc,4)!=4) return 0; crc=lltl(crc);
      if (pk.read(&fn_len,1)!=1) return 0;
      if (pk.read((uint8_t *)fn,fn_len)!=fn_len) return 0;
      if (pk.read((uint8_t *)&fn_len,1)!=1) return 0;
      if (pk.read((uint8_t *)perm,fn_len)!=fn_len) return 0;      // read permission string
      dprintf("nfs open %s,%s\n",fn,perm);
      packet opk;
      int fail;
      uint32_t my_crc=crc_manager.get_crc(crc_manager.get_filenumber(fn),fail);
      if (fail)
      {
    jFILE *fp=new jFILE(squash_path(fn,newfn),perm);
    if (fp->open_failure())
    {
      delete fp;
      opk.write_uint32((int32_t)-1);
      if (!c->nd->send(opk)) return 0;
      return 1;
    } else
    {
      my_crc=crc_file(fp);
      crc_manager.set_crc(crc_manager.get_filenumber(fn),my_crc);
      delete fp;
    }
      }

      if (my_crc==crc)
      {
    opk.write_uint32((int32_t)-2);
    if (!c->nd->send(opk)) return 0;
    return 1;
      }

      jFILE *fp=new jFILE(squash_path(fn,newfn),perm);
      if (fp->open_failure())
      {
    delete fp;
    opk.write_uint32((int32_t)-1);
      } else
    opk.write_uint32(c->add_file(fp));
      if (!c->nd->send(opk)) return 0;
      return 1;
    } break;
    case NFS_OPEN :
    {
      uint8_t fn_len;
      char fn[255],newfn[255],perm[255];
      if (pk.read(&fn_len,1)!=1) return 0;
      if (pk.read((uint8_t *)fn,fn_len)!=fn_len) return 0;
      if (pk.read((uint8_t *)&fn_len,1)!=1) return 0;
      if (pk.read((uint8_t *)perm,fn_len)!=fn_len) return 0;      // read permission string
      dprintf("nfs open %s,%s\n",fn,perm);
      packet opk;
      jFILE *fp=new jFILE(squash_path(fn,newfn),perm);
      if (fp->open_failure())
      {
    delete fp;
    opk.write_uint32((int32_t)-1);
      } else
    opk.write_uint32(c->add_file(fp));
      if (!c->nd->send(opk)) return 0;
      return 1;
    } break;
    case NFS_CLOSE :
    {
      int32_t fd;
      if (pk.read((uint8_t *)&fd,4)!=4) return 0;  fd=lltl(fd);
      dprintf("nfs close %d\n",fd);
      if (!c->delete_file(fd))
      {
    dprintf("nfs : bad fd for close\n");
    return 0;
      }
      return 1;
    } break;
    case NFS_READ :
    {
      int32_t fd,size;
      if (pk.read((uint8_t *)&fd,4)!=4) return 0;    fd=lltl(fd);
      if (pk.read((uint8_t *)&size,4)!=4) return 0;  size=lltl(size);
      dprintf("nfs read %d,%d\n",fd,size);
      bFILE *fp=c->get_file(fd);
      uint8_t buf[NFSFILE_BUFFER_SIZE];
      packet opk;
      if (!fp) return 0;
      int total;
      do
      {
    opk.reset();
    int to_read=NFSFILE_BUFFER_SIZE < size ? NFSFILE_BUFFER_SIZE : size;
    total=fp->read(buf,to_read);
    opk.write_uint16(total);
    opk.write(buf,total);
    printf("sending %d bytes\n",total);
    if (!c->nd->send(opk))
    {
      dprintf("failed on write to client\n");
      return 0;
    }
    if (total<to_read) size=0;
    else size-=total;
      } while (size>0 && total);
      return 1;
    } break;

    case NFS_WRITE : // not supported
    { dprintf("got write command..., not good\n");
      return 0;
    } break;
    case NFS_SEEK :
    {
      int32_t fd,off,type;
      if (pk.read((uint8_t *)&fd,4)!=4) return 0;   fd=lltl(fd);
      if (pk.read((uint8_t *)&off,4)!=4) return 0;  off=lltl(off);
      if (pk.read((uint8_t *)&type,4)!=4) return 0; type=lltl(type);
      dprintf("seek %d %d %d\n",fd,off,type);
      bFILE *fp=c->get_file(fd);
      if (!fp) { dprintf("bad fd for seek\n"); return 0; }
      fp->seek(off,type);
      return 1;
    } break;
    case NFS_FILESIZE :
    {
      int32_t fd,off,type;
      if (pk.read((uint8_t *)&fd,4)!=4) return 0;   fd=lltl(fd);
      bFILE *fp=c->get_file(fd);
      if (!fp) return 0;
      packet opk;
      opk.write_uint32(fp->file_size());
      if (!c->nd->send(opk)) return 0;
      return 1;
    } break;
    case NFS_TELL :
    {
      int32_t fd,off,type;
      if (pk.read((uint8_t *)&fd,4)!=4) return 0;   fd=lltl(fd);
      bFILE *fp=c->get_file(fd);
      if (!fp) return 0;
      packet opk;
      opk.write_uint32(fp->tell());
      if (!c->nd->send(opk)) return 0;
      return 1;
    } break;

  }
  return 0;   // bad command,  all above return 1 when complete
}

int nfs_server::service_request()
{
  int ret=0;
  out_socket *c=listen->check_for_connect();   // check for new clients
  if (c)
  {
    nodes=new nfs_server_client_node(c,nodes);
    ret=1;
  }

  nfs_server_client_node *last=NULL;
  for (nfs_server_client_node *nc=nodes; nc; )    // loop through all clients and check for request
  {
    if (nc->nd->ready_to_read())  // request pending?
    {
      packet pk;
      int kill=0;
      do
      {
    int ret;
    if (!nc->nd->get(pk))
          kill=1;
    else if (!process_packet(pk,nc))
      kill=1;
      } while (!kill && nc->nd->ready_to_read());


      if (kill)
      {
    nfs_server_client_node *p=nc;
    nc=nc->next;
    if (last)
      last->next=nc;
    else
      nodes=NULL;
    delete p;
      } else nc=nc->next;
    } else nc=nc->next;
    last=nc;
  }
  return ret;
}

nfs_server::nfs_server(int Port)
{
  nodes=NULL;
  port=Port;
  listen=NULL;
  int i=0;
  do
  {
    current_sock_err=-1;
    listen=create_in_socket(port);
    if (current_sock_err!=-1)
    {
      dprintf("Port %d already in use, trying next up\n",port);
      port++;
      i++;
      delete listen;
    }
  } while (current_sock_err!=-1 && i<10);
  if (current_sock_err!=-1)
  {
    dprintf("Could not create a listening socket!\n");
    exit(0);
  }

}



nfs_server::~nfs_server()
{
  nfs_server_client_node *p;
  while (nodes)
  {
    p=nodes;
    nodes=nodes->next;
    delete p;
  }
  delete listen;
}



