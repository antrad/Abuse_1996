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

#if HAVE_NETWORK

#include "common.h"

#include "netfile.h"
#include "../inc/netface.h"
#include "engine.h"

nfs_client *first_nfs_client=NULL;
remote_file *remote_file_list=NULL;
char default_fs_name[256];    // default file server name (set with parm -fs)

nfs_client::nfs_client(net_socket *sock, int file_fd, nfs_client *next) :
  sock(sock),file_fd(file_fd),next(next),size_to_read(0)
{
  sock->read_selectable();
}


nfs_client::~nfs_client()
{
  delete sock;
  if (file_fd>=0)
    close(file_fd);
}

void secure_filename(char *filename, char *mode)
{
  if (!no_security)
  {
    if (filename[0]=='/') { filename[0]=0; return ; }
    int level=0;
    char *f=filename;
    while (*f)
    {
      if (*f=='/') { f++; level++; }
      else if (*f=='.' && f[1]=='.')
      {
    if (f[3]=='.') while (*f!='.') f++;
    else
    {
      f+=2;
      level--;
    }
      } else f++;

    }
    if (level<0)
      filename[0]=0;
  }
}


int local_address(char *server_name)    // returns 1 if server name is ourself
{
  struct hostent *hn=gethostbyname(server_name);    // first check to see if this address is 127.0.0.1
  if (!hn) return 0;                                // if bad server_name, return false
  char **ip_address=hn->h_addr_list;
  while (*ip_address)
  {
    char *a=*ip_address;
    if (a[0]==127 && a[1]==0 && a[2]==0 && a[3]==1)
      return 1;
    ip_address++;
  }
  char server_ip[4];
  memcpy(server_ip,hn->h_addr_list,4);

  char my_name[100];                              // now check to see if this address is 'hostname'
  gethostname(my_name,100);
  struct hostent *l_hn=gethostbyname(my_name);
  char **l_ip_address=l_hn->h_addr_list;

  while (*l_ip_address)  // local ip_address
  {
    char *a=*l_ip_address;         // scan through all local ip's
    ip_address=hn->h_addr_list;
    while (*ip_address)            // scan through all ip's for server_name
    {
      char *b=server_ip;
      if (a[0]==b[0] && a[1]==b[1] && a[2]==b[2] && a[3]==b[3])    // check for match
        return 1;
      ip_address++;
    }
    l_ip_address++;
  }
  return 0;       // didn't match localhost nor hostname, must be somewhere else
}

int nfs_client::send_read()   // return 0 if failure on socket, not failure to read
{
  if (file_fd>=0 && socket_fd>=0)
  {
    // first make sure the socket isn't 'full'

    struct timeval tv={ 0,0};     // don't wait
    fd_set write_check;
    FD_ZERO(&write_check);
    FD_SET(socket_fd,&write_check);
    select(FD_SETSIZE,NULL,&write_check,NULL,&tv);

    if (FD_ISSET(socket_fd,&write_check))            // ready to write?
    {
      char buf[READ_PACKET_SIZE];
      int16_t read_total;
      int16_t actual;

      do
      {
    read_total=size_to_read>(READ_PACKET_SIZE-2) ? (READ_PACKET_SIZE-2) : size_to_read;
    actual=read(file_fd,buf,read_total);
    actual=lstl(actual);
    if (write(socket_fd,&actual,sizeof(actual))!=sizeof(actual))
    {
      fprintf(stderr,"write failed\n");
      return 0;
    }
    actual=lstl(actual);

    int write_amount=write(socket_fd,buf,actual);
    if (write_amount!=actual)
    {
      fprintf(stderr,"write failed\n");
      return 0;
    }

    size_to_read-=actual;

    FD_ZERO(&write_check);
    FD_SET(socket_fd,&write_check);
    select(FD_SETSIZE,NULL,&write_check,NULL,&tv);

    if (!FD_ISSET(socket_fd,&write_check))
    {
      FD_SET(socket_fd,&master_write_set);      // socket is full, wait for it empty
      FD_CLR(socket_fd,&master_set);            // don't check for reading or process commands till read is done
      return 1;    // not ok to write anymore, try again latter
    }

      } while (size_to_read && actual==read_total);
      size_to_read=0;
      FD_CLR(socket_fd,&master_write_set);       // don't check this socket for write ok
      FD_SET(socket_fd,&master_set);             // check it for reading
      return 1;
    } else
    {
      FD_SET(socket_fd,&master_write_set);      // socket is full, wait for it empty
      FD_CLR(socket_fd,&master_set);            // don't check for reading or process commands till read is done
      return 1;
    }
  }
  return 0;
}


int process_nfs_command(nfs_client *c)
{
  char cmd;
  if (read(c->socket_fd,&cmd,1)!=1) return 0;
  switch (cmd)
  {
    case NFCMD_READ :
    {
      int32_t size;
      if (read(c->socket_fd,&size,sizeof(size))!=sizeof(size)) return 0;
      size=lltl(size);

      c->size_to_read=size;
      return c->send_read();
    } break;
    case NFCMD_CLOSE :
    {
      return 0;
    } break;
    case NFCMD_SEEK :
    {
      int32_t offset;
      if (read(c->socket_fd,&offset,sizeof(offset))!=sizeof(offset)) return 0;
      offset=lltl(offset);
      offset=lseek(c->file_fd,offset,0);
      offset=lltl(offset);
      if (write(c->socket_fd,&offset,sizeof(offset))!=sizeof(offset)) return 0;
      return 1;
    } break;
    case NFCMD_TELL :
    {
      int32_t offset=lseek(c->file_fd,0,SEEK_CUR);
      offset=lltl(offset);
      if (write(c->socket_fd,&offset,sizeof(offset))!=sizeof(offset)) return 0;
      return 1;
    } break;

    default :
    { fprintf(stderr,"net driver : bad command from nfs client\n");
      return 0;
    }
  }
}



void add_nfs_client(int fd)
{
  uint8_t size[2];
  char filename[300],mode[20],*mp;
  if (read(fd,size,2)!=2) { close(fd); return ; }
  if (read(fd,filename,size[0])!=size[0]) { close(fd); return ; }
  if (read(fd,mode,size[1])!=size[1]) { close(fd); return ; }

  fprintf(stderr,"remote request for %s ",filename);

  secure_filename(filename,mode);  // make sure this filename isn't a security risk
  if (filename[0]==0) { fprintf(stderr,"(denied)\n"); close(fd); return ; }

  mp=mode;
  int flags=0;

  while (*mp)
  {
    if (*mp=='w') flags|=O_CREAT|O_RDWR;
    else if (*mp=='r') flags|=O_RDONLY;
    mp++;
  }

  int f=open(filename,flags,S_IRWXU | S_IRWXG | S_IRWXO);

  if (f<0)
  {
    fprintf(stderr,"(not found)\n");
    f=-1;  // make sure this is -1
  }

  int32_t ret=lltl(f);
  if (write(fd,&ret,sizeof(ret))!=sizeof(ret)) { close(fd); return ; }

  if (f<0)    // no file, sorry
    close(fd);
  else
  {
    int32_t cur_pos=lseek(f,0,SEEK_CUR);
    int32_t size=lseek(f,0,SEEK_END);
    lseek(f,cur_pos,SEEK_SET);
    size=lltl(size);
    if (write(fd,&size,sizeof(size))!=sizeof(size)) {  close(f); close(fd); return ; }

    first_nfs_client=new nfs_client(fd,f,first_nfs_client);
    first_nfs_client->size=size;
  }
}

void remote_file::r_close(char *reason)
{
  if (reason)
    fprintf(stderr,"remote_file : %s\n",reason);
  if (socket_fd>=0)
  {
    uint8_t cmd=NFCMD_CLOSE;
    write(socket_fd,&cmd,1);
    close(socket_fd);
  }
  socket_fd=-1;
}

remote_file::remote_file(char *filename, char *mode, remote_file *Next)
{
  next=Next;
  open_local=0;

  socket_fd=connect_to_server(filename);
  if (socket_fd==-1)
  {
    fprintf(stderr,"unable to connect\n");
    return ;
  }

  uint8_t sizes[3]={ CLIENT_NFS,strlen(filename)+1,strlen(mode)+1};
  if (write(socket_fd,sizes,3)!=3) { r_close("could not send open info"); return ; }
  if (write(socket_fd,filename,sizes[1])!=sizes[1]) { r_close("could not send filename"); return ; }
  if (write(socket_fd,mode,sizes[2])!=sizes[2]) { r_close("could not send mode"); return ; }

  int32_t remote_file_fd;
  if (read(socket_fd,&remote_file_fd,sizeof(remote_file_fd))!=sizeof(remote_file_fd))
  { r_close("could not read remote fd"); return ; }
  remote_file_fd=lltl(remote_file_fd);
  if (remote_file_fd<0) { r_close("remote fd is bad"); return ; }

  if (read(socket_fd,&size,sizeof(size))!=sizeof(size)) { r_close("could not read remote filesize"); return ; }
//  uint32_t remote_crc;
//  if (read(socket_fd,&remote_crc,sizeof(remote_crc))!=sizeof(remote_crc)) { r_close("could not read remote checksum"); return ; }
//  uint32_t local_crc=

  size=lltl(size);
}

int remote_file::unbuffered_read(int out_fd, size_t count)
{
  if (socket_fd>=0 && count)
  {
    uint8_t cmd=NFCMD_READ;
    if (write(socket_fd,&cmd,sizeof(cmd))!=sizeof(cmd)) { r_close("read : could not send command"); return 0; }

    int32_t rsize=lltl(count);
    if (write(socket_fd,&rsize,sizeof(rsize))!=sizeof(rsize)) { r_close("read : could not send size"); return 0; }

    int32_t total_read=0,total;
    char buf[READ_PACKET_SIZE];
    uint16_t size;

    uint16_t packet_size;
    do
    {
      if (read(socket_fd,&packet_size,sizeof(packet_size))!=sizeof(packet_size))
      {
    fprintf(stderr,"could not read packet size\n");
    return 0;
      }
      packet_size=lstl(packet_size);

      uint16_t size_read=read(socket_fd,buf+2,packet_size);

      if (size_read!=packet_size)
      {
    if (read(socket_fd,buf+2+size_read,packet_size-size_read)!=packet_size-size_read)
    {
      fprintf(stderr,"incomplete packet\n");
      return 0;
    }
      }

      *((int16_t *)buf)=packet_size;
      if (write(out_fd,buf,packet_size+2)!=packet_size+2) comm_failed();

      total_read+=packet_size;
      count-=packet_size;
    } while (packet_size==READ_PACKET_SIZE-2 && count);
    return total_read;
  }
  return 0;
}

int remote_file::unbuffered_tell()   // ask server where the offset of the file pointer is
{
  if (socket_fd>=0)
  {
    uint8_t cmd=NFCMD_TELL;
    if (write(socket_fd,&cmd,sizeof(cmd))!=sizeof(cmd)) { r_close("tell : could not send command"); return 0; }

    int32_t offset;
    if (read(socket_fd,&offset,sizeof(offset))!=sizeof(offset)) { r_close("tell : could not read offset"); return 0; }
    return lltl(offset);
  }
  return 0;
}

int remote_file::unbuffered_seek(int32_t offset)  // tell server to seek to a spot in a file
{
  if (socket_fd>=0)
  {
    uint8_t cmd=NFCMD_SEEK;
    if (write(socket_fd,&cmd,sizeof(cmd))!=sizeof(cmd)) { r_close("seek : could not send command"); return 0; }

    int32_t off=lltl(offset);
    if (write(socket_fd,&off,sizeof(off))!=sizeof(off)) { r_close("seek : could not send offset"); return 0; }

    if (read(socket_fd,&offset,sizeof(offset))!=sizeof(offset)) { r_close("seek : could not read offset"); return 0; }
    return lltl(offset);
  }
  return 0;
}

int open_file(char *&filename, char *mode)
{
  if (filename[0]!='/' && filename[1]!='/' && default_fs_name[0])   // default file server?
  {
    char tmp_fn[500];
    sprintf(tmp_fn,"//%s/%s",default_fs_name,filename);
    strcpy(filename,tmp_fn);
  }

  if (filename[0]=='/' && filename[1]=='/')   // passive server file reference?
  {
    filename+=2;
    remote_file *rf=new remote_file(filename,mode,remote_file_list);
    if (rf->open_failure())
    {
      delete rf;
      return -1;
    }
    else
    {
      remote_file_list=rf;
      return rf->socket_fd;
    }
  }

  secure_filename(filename,mode);
  if (filename[0]==0) return -1;

  int flags=0;
  while (*mode)
  {
    if (*mode=='w') flags|=O_CREAT|O_RDWR;
    else if (*mode=='r') flags|=O_RDONLY;
    mode++;
  }

  int f=open(filename,flags,S_IRWXU | S_IRWXG | S_IRWXO);
  if (f>=0)
  { close(f);
    return -2;
  }

  return -1;
}

remote_file *find_rfile(int fd)
{
  remote_file *r=remote_file_list;
  for (; r && r->socket_fd!=fd; r=r->next)
  {
    if (r->socket_fd==-1)
    {
      fprintf(stderr,"bad sock\n");
    }
  }
  return r;
}

void unlink_remote_file(remote_file *rf)
{
  if (rf==remote_file_list)
    remote_file_list=rf->next;
  else
  {
    remote_file *last=remote_file_list;
    while (last->next && last->next!=rf) last=last->next;
    last->next=rf->next;
  }
}

remote_file::~remote_file()
{ r_close(NULL); }


int fetch_crcs(char *server)
{
  int socket_fd=connect_to_server(server);
  if (socket_fd==-1)
  {
    fprintf(stderr,"unable to connect\n");
    return 0;
  }

  uint8_t cmd=CLIENT_CRC_WAITER;
  if (write(socket_fd,&cmd,1)!=1)  { close(socket_fd); return 0; }
  if (read(socket_fd,&cmd,1)!=1)  { close(socket_fd); return 0; }
  close(socket_fd);
  return cmd;

}

#endif // HAVE_NETWORK

