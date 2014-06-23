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
#include <sys/ipc.h>
#include <sys/shm.h>

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/fcntl.h>
#include <fcntl.h>

#include "netface.h"
#include "nfserver.h"
#include "view.h"
#include "objects.h"
#include "level.h"
#include "timing.h"
#include "server2.h"
#include "game.h"
#include "jwindow.h"

extern char lsf[256];

#define DIN_NAME "/tmp/.abuse_ndrv_out"   // opposite of driver's in/out
#define DOUT_NAME "/tmp/.abuse_ndrv_in"

#define real2shm(type,ptr) (ptr==NULL ? NULL : ((type *)((uint8_t *)(ptr)-(uint8_t *)base)))
#define shm2real(type,ptr) (ptr==NULL ? NULL : ((type *)((intptr_t)(ptr)+(intptr_t)(base))))

#ifdef __sgi
#define next_process() sginap(0)
#else
#define next_process() usleep(1)
#endif

extern int crc_man_write_crc_file(char const *filename);
int net_installed=0,net_out_fd,net_in_fd;
int net_child=-1;
int local_client_number=0;

void *shm_addr=(void *)-1;  // shmat returns -1 on failure
base_memory_struct *base;   // points to shm_addr
base_memory_struct local_base;

net_address *net_server=NULL;

int net_start()
{
  return net_server!=NULL;
}


int kill_net()
{

  if (net_installed)
  {
    char cmd=EGCMD_DIE;
    write(net_out_fd,&cmd,1);
    read(net_in_fd,&cmd,1);
    close(net_out_fd);
    close(net_in_fd);
  }

  if (net_child!=-1)
  {
    kill(net_child,SIGINT);
    net_child=-1;
  }

  net_installed=0;
  if (shm_addr!=(void *)-1)
  {
    shmdt((char *)shm_addr);
    shm_addr=(void *)-1;
    base=&local_base;
  }
}

void net_uninit()
{
  kill_net();
}

int net_init(int argc, char **argv)
{
  int i,p1=-1,no_fork=0;
  base=&local_base;
  char *server_name=NULL;

  for (i=1; i<argc; i++)
    if (!strcmp(argv[i],"-nonet"))
      return 0;
    else if (!strcmp(argv[i],"-no_net_fork"))      // make it easy to run the driver under a debugger
      no_fork=1;
    else if (!strcmp(argv[i],"-port"))
    {
      if (i==argc-1 || !sscanf(argv[i+1],"%d",&p1))
      {
        fprintf(stderr,"bad value folloing -port");
    return 0;
      }
    } else if (!strcmp(argv[i],"-net") && i<argc-1)
    {
      i++;
      server_name=argv[i];
    }



  char cmd[50];
  if (p1!=-1)
    sprintf(cmd,"undrv runme -port %d",p1);
  else sprintf(cmd,"undrv runme");

  if (!no_fork)
  {
    FILE *fp=popen(cmd,"rb");
    if (!fp || !fscanf(fp,"%d",&net_child) || net_child==-1)
    { fprintf(stderr,"could not run undrv, please make sure it's in your path\n");
      return 0;
    }

    if (fp) pclose(fp);
  }

  do
  { sleep(0);
  } while (access(DIN_NAME,R_OK));
  net_in_fd=open(DIN_NAME,O_RDWR);

  do
  { sleep(0);
  } while (access(DOUT_NAME,W_OK));
  net_out_fd=open(DOUT_NAME,O_RDWR);


  uint8_t reg = 1;
  if (write(net_out_fd,&reg,sizeof(reg))!=sizeof(reg))
    return 0;

  int shm_seg_id;
  if (read(net_in_fd,&shm_seg_id,sizeof(shm_seg_id))!=sizeof(shm_seg_id))
    return 0;

  shm_addr=shmat(shm_seg_id,NULL,0);  // attach as read/write
  if (shm_addr==(void *)-1)
    return 0;

  char ack=1;   // acknodge we read and attached
  if (write(net_out_fd,&ack,1)!=1)
    return 0;


  base=(base_memory_struct *)shm_addr;

  net_installed=1;

  return 1;
}

#include <unistd.h>
#include <sys/time.h>

int NF_set_file_server(char *name)
{
  if (net_installed)
  {
    char cm[2]={ NFCMD_SET_FS,strlen(name)+1};
    if (write(net_out_fd,cm,2)!=2) { kill_net(); return 0; }
    if (write(net_out_fd,name,cm[1])!=cm[1]) { kill_net(); return 0; }
    if (read(net_in_fd,cm,1)!=1)  { kill_net(); return 0; }   // read the status of this command
    next_process();
    return cm[0];
  } else return 0;
}

int NF_open_file(char const *filename, char const *mode)
{
  if (net_installed)
  {
    char cm[3]={ NFCMD_OPEN,strlen(filename)+1,strlen(mode)+1};
    if (write(net_out_fd,cm,3)!=3) { kill_net(); return -1; }
    if (write(net_out_fd,filename,cm[1])!=cm[1])  { kill_net(); return -1; }
    if (write(net_out_fd,mode,cm[2])!=cm[2])  { kill_net(); return -1; }

    uint8_t file_type;
    if (read(net_in_fd,&file_type,1)!=1)  { kill_net(); return -1; }
    if (file_type==NF_OPEN_LOCAL_FILE)
    {
      uint8_t name_size;
      if (read(net_in_fd,&name_size,1)!=1)  { kill_net(); return -1; }
      int size=read(net_in_fd,filename,name_size);
      if (size!=name_size)  { kill_net(); return -1; }
      return -2;
    }
    else if (file_type==NF_OPEN_FAILED) return -1;

    int fd;
    if (read(net_in_fd,&fd,sizeof(fd))!=sizeof(fd))  { kill_net(); return -1; }

    return fd;
  } else return -2;          // return open local
}

int32_t NF_close(int fd)
{
  if (net_installed)
  {
    char cm=NFCMD_CLOSE;
    if (write(net_out_fd,&cm,1)!=1) { kill_net(); return 0; }
    if (write(net_out_fd,&fd,sizeof(fd))!=sizeof(fd)) { kill_net(); return 0; }
    char stat;
    if (read(net_in_fd,&stat,sizeof(stat))!=sizeof(stat))  { kill_net(); return 0; }
    return stat;
  } else return 0;
}


int32_t NF_read(int fd, void *buf, int32_t size)
{
  if (net_installed && size)
  {
    char cm=NFCMD_READ;

    if (write(net_out_fd,&cm,1)!=1) { kill_net(); return 0; }
    if (write(net_out_fd,&fd,sizeof(fd))!=sizeof(fd)) { kill_net(); return 0; }
    if (write(net_out_fd,&size,sizeof(size))!=sizeof(size)) { kill_net(); return 0; }

    int32_t total_read=0;
    uint16_t t=0xffff;
    while (size && t>=READ_PACKET_SIZE-2)
    {
      if (read(net_in_fd,&t,sizeof(t))!=sizeof(t))  { kill_net(); return 0; }
      if (read(net_in_fd,buf,t)!=t)  { kill_net(); return total_read; }

      total_read+=t;
      size-=t;
      buf=(void *)((char *)buf+t);


    }
    return total_read;
  } else return 0;
}


int32_t NF_filelength(int fd)
{
  if (net_installed)
  {
    char cm=NFCMD_SIZE;
    if (write(net_out_fd,&cm,1)!=1) { kill_net(); return 0; }
    if (write(net_out_fd,&fd,sizeof(fd))!=sizeof(fd)) { kill_net(); return 0; }
    int32_t size;
    if (read(net_in_fd,&size,sizeof(size))!=sizeof(size))  { kill_net(); return 0; }
    return size;
  } else return 0;
}

int32_t NF_tell(int fd)
{
  if (net_installed)
  {
    char cm=NFCMD_TELL;
    if (write(net_out_fd,&cm,1)!=1) { kill_net(); return 0; }
    if (write(net_out_fd,&fd,sizeof(fd))!=sizeof(fd)) { kill_net(); return 0; }
    int32_t offset;
    if (read(net_in_fd,&offset,sizeof(offset))!=sizeof(offset))  { kill_net(); return 0; }
    return offset;
  } else return 0;
}

int32_t NF_seek(int fd, int32_t offset)
{
  if (net_installed)
  {
    char cm=NFCMD_SEEK;
    if (write(net_out_fd,&cm,1)!=1) { kill_net(); return 0; }
    if (write(net_out_fd,&fd,sizeof(fd))!=sizeof(fd)) { kill_net(); return 0; }
    if (write(net_out_fd,&offset,sizeof(offset))!=sizeof(offset)) { kill_net(); return 0; }

    int32_t offset;
    if (read(net_in_fd,&offset,sizeof(offset))!=sizeof(offset))  { kill_net(); return 0; }
    return offset;
  } else return 0;
}

static int aquire_mem_lock()
{
  if (base->mem_lock==0 || base->mem_lock==2)
  {
    base->mem_lock=2;
    if (base->mem_lock==2)
      return 1;
  }
//  next_process();   // probably just gonna loop until we get the lock so halt for next preocess
  return 0;
}

void service_net_request()
{
  if (net_installed)
  {
    if (base->input_state==INPUT_NET_DEAD)
      kill_net();
    else
    {
      if (aquire_mem_lock())
      {
    if (base->calc_crcs)
    {
      crc_man_write_crc_file(NET_CRC_FILENAME);       // return 0 on failure
      base->calc_crcs=0;
      base->mem_lock=0;

      uint8_t cmd=NFCMD_CRCS_CALCED;
      if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return ; }
    } else base->mem_lock=0;
      }
      if (aquire_mem_lock())
      {
    if (base->get_lsf)
    {
      base->get_lsf=0;
      base->mem_lock=0;
      uint8_t c[2]={ NFCMD_PROCESS_LSF,strlen(lsf)+1};
      if (write(net_out_fd,&c,2)!=2) { kill_net(); return ; }
      if (write(net_out_fd,lsf,c[1])!=c[1]) { kill_net(); return ; }
    } else base->mem_lock=0;
      }
    }
  }
}


int get_remote_lsf(char *name, char *filename)  // filename should be 256 bytes
{
  if (net_installed)
  {
    uint8_t cm[2]={ NFCMD_REQUEST_LSF,strlen(name)+1};
    if (write(net_out_fd,cm,2)!=2) { kill_net(); return 0; }
    if (write(net_out_fd,name,cm[1])!=cm[1]) { kill_net(); return 0; }
    uint8_t size;
    if (read(net_in_fd,&size,1)!=1) { kill_net(); return 0; }
    if (size==0) return 0;
    if (read(net_in_fd,filename,size)!=size) { kill_net(); return 0; }
    return 1;
  } else return 0;
}

int request_server_entry()
{
  if (net_installed)
  {
    if (!net_server) return 0;
    uint8_t cm[2]={ NFCMD_REQUEST_ENTRY,strlen(net_server)+1};
    if (write(net_out_fd,cm,2)!=2) { kill_net(); return 0; }
    if (write(net_out_fd,net_server,cm[1])!=cm[1]) { kill_net(); return 0; }
    uint16_t cnum;  // client number
    if (read(net_in_fd,&cnum,2)!=2) { kill_net(); return 0; }
    if (cnum==0) return 0;
    local_client_number=cnum;
    return 1;
  } else return 0;
}


int reload_start()
{
  if (net_installed)
  {
    uint8_t cmd=NFCMD_RELOAD_START;
    if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return 0; }
    if (read(net_in_fd,&cmd,1)!=1) { kill_net(); return 0; }
    return cmd;
  } else return 1;
}


int reload_end()
{
  if (net_installed)
  {
    uint8_t cmd=NFCMD_RELOAD_END;
    if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return 0; }
    if (read(net_in_fd,&cmd,1)!=1) { kill_net(); return 0; }
    return cmd;
  } else return 1;
}

void net_reload()
{
  if (net_installed)
  {
    if (net_server)
    {
      if (current_level)
        delete current_level;
      bFILE *fp;

      if (!reload_start()) return ;

      do {            // make sure server saves the file
    fp=open_file(NET_STARTFILE,"rb");
    if (fp->open_failure()) { delete fp; fp=NULL; }
      } while (!fp);

      spec_directory sd(fp);

#if 0
      spec_entry *e=sd.find("Copyright 1995 Crack dot Com, All Rights reserved");
      if (!e)
      {
    the_game->show_help("This level is missing copyright information, cannot load\n");
    current_level=new level(100,100,"untitled");
    the_game->need_refresh();
      }
      else
#endif
        current_level=new level(&sd,fp,NET_STARTFILE);

      delete fp;
      base->current_tick=(current_level->tick_counter()&0xff);

      reload_end();
    } else if (current_level)
    {

      join_struct *join_list=shm2real(join_struct,base->join_list);

      while (!aquire_mem_lock())
      {
    next_process();
    service_net_request();
      }

      while (join_list)
      {

    view *f=player_list;
    for (; f && f->next; f=f->next);      // find last player, add one for pn
    int i,st=0;
    for (i=0; i<total_objects; i++)
    if (!strcmp(object_names[i],"START"))
    st=i;

    game_object *o=create(current_start_type,0,0);
    game_object *start=current_level->get_random_start(320,NULL);
    if (start) { o->x=start->x; o->y=start->y; }
    else { o->x=100; o->y=100; }

    f->next=new view(o,NULL,shm2real(join_struct,base->join_list)->client_id);
    strcpy(f->next->name,shm2real(join_struct,base->join_list)->name);
    o->set_controller(f->next);

    if (start)
    current_level->add_object_after(o,start);
    else
    current_level->add_object(o);

    view *v=f->next;

    v->cx1=5;
    v->cy1=5;
    v->cx2=319-5;
    v->cy2=199-5;
    join_list=shm2real(join_struct,join_list->next);
      }
      base->join_list=NULL;
      current_level->save(NET_STARTFILE,1);
      base->mem_lock=0;


      Jwindow *j=wm->new_window(0,yres/2,-1,-1,new info_field(0, 0, 0, "Clients are re-syncing, please wait...",NULL));
      wm->flush_screen();
      if (!reload_start()) return ;

      // wait for all client to reload the level with the new players
      do
      {
    next_process();
      } while (!reload_end());
      wm->close_window(j);

    }
  }
}

int client_number() { return local_client_number; }


void send_local_request()
{
  if (net_installed)
  {
    if (base->join_list)
      base->packet.write_uint8(SCMD_RELOAD);

    uint8_t cmd=NFCMD_SEND_INPUT;

    if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return ; }
    if (read(net_in_fd,&cmd,1)!=1) { kill_net(); return ; }
    if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return ; }
  } else base->input_state=INPUT_PROCESSING;
}

void kill_slackers()
{
  if (net_installed)
  {
    uint8_t cmd=NFCMD_KILL_SLACKERS;
    if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return ; }
    if (read(net_in_fd,&cmd,1)!=1) { kill_net(); return ; }
  }
}

int get_inputs_from_server(unsigned char *buf)
{
  if (net_installed && base->input_state!=INPUT_PROCESSING)      // if input is not here, wait on it
  {
    timeval start;
    gettimeofday(&start,NULL);

    int total_retry=0;
    Jwindow *abort=NULL;
    linked_list input;
    while (base->input_state!=INPUT_PROCESSING)
    {
      if (!net_installed)
      {
    base->input_state=INPUT_PROCESSING;
    return 1;
      }
      server_check();
      service_net_request();

      timeval now;                   // if this is taking to long, the packet was probably lost, ask for it to be resent
      gettimeofday(&now,NULL);
      if ((((now.tv_sec-start.tv_sec)*100)+(now.tv_usec-start.tv_usec)/10000)>20)
      {
//    fprintf(stderr,"receive timeout %d\n",(((now.tv_sec-start.tv_sec)*100)+(now.tv_usec-start.tv_usec)/10000));
    uint8_t cmd=NFCMD_INPUT_MISSING;
    if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return  0; }
    if (read(net_in_fd,&cmd,1)!=1) { kill_net(); return 0; }     // block, so net driver can request input
    gettimeofday(&start,NULL);
    total_retry++;
    if (total_retry==10)    // 2 seconds and nothing
    {
      abort=wm->new_window(0,yres/2,-1,wm->font()->height()*4,
                   new info_field(0, 0, 0,"Waiting for data...",
                          new button(0, wm->font()->height()+5,ID_NET_DISCONNECT,
                             "Disconnect slackers",NULL)),"Error");
      wm->flush_screen();
    }
      }
      if (abort)
      {
    if (wm->event_waiting())
    {
      event ev;
      do
      {
        wm->get_event(ev);
        if (ev.type==EV_MESSAGE && ev.message.id==ID_NET_DISCONNECT)
        kill_slackers();
        else if (ev.type!=EV_MOUSE_MOVE)  // no need to save mouse move events (likely to be a lot)
        {
          event *e=new event;
          *e=ev;
          input.add_front(e);
        }
      } while (wm->event_waiting());

      wm->flush_screen();
    }
      }
    }

    if (abort)
    {
      wm->close_window(abort);
      while (input.first())               // push all the key events
      {
    event *ev=(event *)input.first();
    input.unlink(ev);
    wm->push_event(ev);
      }
    }
  }

//  while (!aquire_mem_lock()) service_net_request();

  memcpy(base->last_packet.data,base->packet.data,base->packet.packet_size()+base->packet.packet_prefix_size());

  int size=base->packet.packet_size();
  memcpy(buf,base->packet.packet_data(),size);

  base->packet.packet_reset();
  base->mem_lock=0;

  return size;
}


void server_check()       // read a byte from the net driver, causing the OS to give up the rest of our time-slice
{
  if (net_installed)
  {
    if (base->input_state==INPUT_NET_DEAD)
    { close(net_out_fd); close(net_in_fd); net_installed=0; kill_net(); }
    else
    {
      uint8_t cmd=NFCMD_BLOCK;
      if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return ; }
      if (base->input_state==INPUT_NET_DEAD)
      { close(net_out_fd); close(net_in_fd); net_installed=0; kill_net(); }
      else
      {
    if (read(net_in_fd,&cmd,1)!=1) { kill_net(); return ; }
    if (base->input_state==INPUT_NET_DEAD)
    { close(net_out_fd); close(net_in_fd); net_installed=0; kill_net(); }
    else
      if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return ; }
      }
    }
  }
}

int become_server()
{
  if (net_installed)
  {
    uint8_t cmd=NFCMD_BECOME_SERVER;
    if (write(net_out_fd,&cmd,1)!=1) { kill_net(); return 0; }
    if (read(net_in_fd,&cmd,1)!=1) { kill_net(); return 0; }

    return 1;
  }
  return 0;
}

void read_new_views() { ; }


