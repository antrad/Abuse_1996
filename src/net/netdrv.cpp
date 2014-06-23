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

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <bstring.h>
#include <netdb.h>

#include "fileman.h"
#include "netdrv.h"
#include "gserver.h"
#include "gclient.h"
#include "undrv.h"
#include "tcpip.h"

#define real2shm(type,ptr) (ptr==NULL ? NULL : ((type *)((uint8_t *)(ptr)-(uint8_t *)base)))
#define shm2real(type,ptr) (ptr==NULL ? NULL : ((type *)((intptr_t)(ptr)+(intptr_t)(base))))

net_driver *driver=NULL;

#ifdef __sgi
#define next_process() sginap(0)
#else
#define next_process() usleep(10)
#endif


#ifdef __sgi
void die(...)
#else
void die(int why)
#endif
{
  fprintf(stderr,"dieing\n");
  if (driver) { delete driver; driver=NULL; }
  exit(0);
}


void mdie(char *reason)
{
  fprintf(stderr,"net driver : %s\n",reason);
  if (driver) { driver->cleanup(); }
  exit(0);
}

void comm_failed()  // general communication failure with engine
{
  fprintf(stderr,"net driver : Error occurred while trying to communicate with the engine\n");
  if (driver) { delete driver; driver=NULL; }
  exit(0);
}

int net_driver::add_joiner(int client_id, char *name)
{
  join_array[client_id].next=base->join_list;
  base->join_list=real2shm(join_struct,&join_array[client_id]);
  join_array[client_id].client_id=client_id;
  strcpy(join_array[client_id].name,name);
}


void net_driver::cleanup()
{
  base->input_state=INPUT_NET_DEAD;
  fprintf(stderr,"net driver : cleaning up\n");
  if (shm_seg_id!=-1)
    shmctl(shm_seg_id,IPC_RMID,NULL);
  if (shm_addr!=(void *)-1)
  {
    shmdt((char *)shm_addr);
    shm_addr=(void *)-1;
  }

  undrv_cleanup();
  unlink(DLOCK_NAME);
}

net_driver::~net_driver()
{
  cleanup();
}

int net_driver::setup_shm()
{
  shm_addr=(void *)-1;  // shmat returns -1 on failure
  shm_seg_id=-1;

  driver=this;
  int catch_sigs[]={ SIGHUP,SIGINT,SIGQUIT,SIGILL,SIGABRT,
            SIGIOT,SIGFPE,SIGKILL,SIGUSR1,SIGSEGV,
            SIGUSR2,SIGPIPE,SIGTERM,SIGCHLD,
            SIGCONT,SIGSTOP,SIGTSTP,SIGTTIN,SIGTTOU,-1};

  int i;
  for (i=0; catch_sigs[i]!=-1; i++)     // catch all signals in case we get
    signal(catch_sigs[i],die);            // interrupted before we remove shmid


  int alloc_size=sizeof(join_struct)*MAX_JOINERS+sizeof(base_memory_struct);

  shm_seg_id=shmget(IPC_PRIVATE,alloc_size,IPC_CREAT | 0777);


  if (shm_seg_id==-1) mdie("Unable to allocate shared memory");


  shm_addr=shmat(shm_seg_id,NULL,0);  // attach as read/write
  if (shm_addr==(void *)-1)
    mdie("could not attach shm seg");

  base=(base_memory_struct *)shm_addr;

  base->join_list=real2shm(join_struct,NULL);
  base->mem_lock=0;
  base->calc_crcs=0;
  base->get_lsf=0;
  base->wait_reload=0;
  base->need_reload=0;
  base->input_state=INPUT_COLLECTING;
  base->current_tick=0;
  base->packet.packet_reset();
  join_array=(join_struct *) (base+1);

  // see if we can attach this memory with the abuse engine
  if (out->write(&shm_seg_id,sizeof(shm_seg_id))!=sizeof(shm_seg_id))
    comm_failed();

  // wait for engine to ack it has attached
  uint8_t ack=0;
  if (in->read(&ack,1)!=1 || ack!=1)
    comm_failed();


  if (shmctl(shm_seg_id,IPC_RMID,NULL))  // remove the shm id
    mdie("could not remove shm id");

  shm_seg_id=-1;                      // mark id as not allocated
  return 1;

}

int net_driver::connect_to_engine(int argc, char **argv)
{
  if (mkfifo(DIN_NAME,S_IRWXU | S_IRWXG | S_IRWXO))
  { perror("Net driver : unable to make fifo in /tmp");
    return 0;
  }
  chmod(DIN_NAME,S_IRWXU | S_IRWXG | S_IRWXO);   // just to be sure umask doesn't screw us

  if (mkfifo(DOUT_NAME,S_IRWXU | S_IRWXG | S_IRWXO))
  { perror("Net driver : unable to make fifo in /tmp");
    return 0;
  }
  chmod(DOUT_NAME,S_IRWXU | S_IRWXG | S_IRWXO);

  int driver_out_fd=open(DOUT_NAME,O_RDWR);  // open the pipe
  if (driver_out_fd<0)
  { perror(DOUT_NAME);
    exit(1);
  }

  int driver_in_fd=open(DIN_NAME,O_RDWR);
  if (driver_in_fd<0)
  {
    close(driver_out_fd);
    perror(DIN_NAME);
    exit(1);
  }

  in=new unix_fd(driver_in_fd);
  in->read_selectable();
  out=new unix_fd(driver_out_fd);

  if (in->read(&reg,sizeof(reg))!=sizeof(reg))
    mdie("unable to registration from engine");
}

net_driver::net_driver(int argc, char **argv, int comm_port, int game_port, net_protocol *proto) :
  comm_port(comm_port), game_port(game_port), proto(proto)
{
  debug=0;
  lsf_wait_list=NULL;
  crc_wait_list=NULL;

  base=NULL;
  in=out=NULL;
  game_face=new game_handler();

  connect_to_engine(argc,argv);
  setup_shm();
  int i;
  for (i=1; i<argc; i++) if (!strcmp(argv[i],"-debug")) debug=1;
}

int net_driver::become_server()
{
  delete game_face;
  game_face=new game_server;
  return 1;
}

int net_driver::check_commands()
{
  int ret=0;
  if (in->ready_to_read())       // commands from engine?
  {
    uint8_t cmd;
    if (in->read(&cmd,1)!=1) return 0;

    if (debug)
    {
      if (cmd<=EGCMD_DIE)
      {
    char *cmds[]={ "open","close","read","write","seek","size","tell","setfs","crc_calced","process_lsf","request_lfs",
             "equest_entry","become_server","block","reload_start","reload_end","send_input","input_missing",
              "kill_slackers","die"};
    fprintf(stderr,"engine cmd : %s\n",cmds[cmd]);
      }
    }

    switch (cmd)
    {
      case EGCMD_DIE :
      {
    cmd=game_face->quit();
    if (!out->write(&cmd,1)) { mdie("could not write block ack1"); }  // send something to unblock engine
    mdie("received die command");
      } break;

      case NFCMD_RELOAD_START :
      {
    cmd=game_face->start_reload();
    if (!out->write(&cmd,1)) { mdie("could not write start reload ack"); }  // send something to unblock engine
      } break;

      case NFCMD_RELOAD_END :
      {
    cmd=game_face->end_reload();
    if (!out->write(&cmd,1)) { mdie("could not write end reload ack"); }  // send something to unblock engine
      } break;

      case NFCMD_BLOCK :
      {
    if (!out->write(&cmd,1)) { mdie("could not write block ack1"); }  // send something to unblock engine
    if (!in->read(&cmd,1)) { mdie("could not read block ack1"); }  // send something to block ourself
      } break;

      case NFCMD_INPUT_MISSING :    // try to fetch the input via a loss-less net protocol
      {
    game_face->input_missing();
    if (out->write(&cmd,1)!=1) { mdie("could not write block ack1"); }  // send something to unblock engine
      } break;

      case NFCMD_KILL_SLACKERS :
      {
    if (!game_face->kill_slackers())
    {
      delete game_face;
      game_face=new game_handler();
    }
    if (out->write(&cmd,1)!=1) { mdie("could not write block ack1"); }  // send something to unblock engine
      } break;
      case NFCMD_SEND_INPUT :
      {
    game_face->add_engine_input();
    if (out->write(&cmd,1)!=1) { mdie("could not write send ack1"); }  // send something to unblock engine
    if (in->read(&cmd,1)!=1) { mdie("could not read send ack2"); }    // read something to block ourselves for engine
      } break;

      case NFCMD_REQUEST_ENTRY :
      {
    uint8_t len;
    char name[256];
    if (in->read(&len,1)!=1) { mdie("could not read server name length"); }
    if (in->read(name,len)!=len) { mdie("could not read server name"); }
    uint16_t success=join_server(name);
    if (out->write(&success,2)!=2) mdie("cound not send lsf read failure");
      } break;

      case NFCMD_BECOME_SERVER :
      {
    cmd=become_server();
    if (out->write(&cmd,1)!=1) mdie("cound not send ok");
      } break;

      case NFCMD_REQUEST_LSF :
      {
    uint8_t len;
    char name[256];
    if (in->read(&len,1)!=1) { mdie("could not read lsf name length"); }
    if (in->read(name,len)!=len) { mdie("could not read lsf name"); }
    if (!get_lsf(name))
    {
      len=0;
      if (out->write(&len,1)!=1) mdie("cound not send lsf read failure");
    } else
    {
      len=strlen(name)+1;
      if (out->write(&len,1)!=1) mdie("cound not send lsf name len");
      if (out->write(name,len)!=len) mdie("cound not send lsf name");
    }
      } break;

      case NFCMD_PROCESS_LSF :
      {
    uint8_t len,name[256];
    if (in->read(&len,1)!=1) { mdie("could not read lsf name length"); }
    if (in->read(name,len)!=len) { mdie("could not read lsf name"); }

    while (lsf_wait_list)
    {
      lsf_waiter *c=lsf_wait_list;
      lsf_wait_list=lsf_wait_list->next;
      uint8_t status=1;
      c->sock->write(&len,1);
      c->sock->write(name,len);
      delete c;
    }
      } break;

      case NFCMD_CRCS_CALCED :
      {
    while (crc_wait_list)
    {
      crc_waiter *c=crc_wait_list;
      crc_wait_list=crc_wait_list->next;
      uint8_t status=1;
      c->sock->write(&status,1);
      delete c;
    }
      } break;

      case NFCMD_SET_FS :
      {
    uint8_t size;
    char sn[256];
    if (in->read(&size,1)!=1) mdie("could not read filename length");
    if (in->read(sn,size)!=size) mdie("could not read server name");
    fman->set_default_fs_name(sn);

    size=fetch_crcs(sn);  // return success
    if (out->write(&size,1)!=1) mdie("could not send ok to engine");
      } break;

      case NFCMD_OPEN :
      {
    uint8_t size[2];
    char filename[300],mode[20],*fn;
    fn=filename;
    if (in->read(size,2)!=2  ||
        in->read(filename,size[0])!=size[0] ||
        in->read(mode,size[1])!=size[1])
      mdie("incomplete open command from engine");

    int fd=fman->rf_open_file(fn,mode);
    if (fd==-2)
    {
      uint8_t st[2];
      st[0]=NF_OPEN_LOCAL_FILE;
      st[1]=strlen(fn)+1;
      if (out->write(st,2)!=2) comm_failed();
      if (out->write(fn,st[1])!=st[1]) comm_failed();
    } else if (fd==-1)
    {
      uint8_t st=NF_OPEN_FAILED;
      if (out->write(&st,1)!=1) comm_failed();
    } else
    {
      uint8_t st=NF_OPEN_REMOTE_FILE;
      if (out->write(&st,1)!=1) comm_failed();
      if (out->write(&fd,sizeof(fd))!=sizeof(fd)) comm_failed();
    }
      } break;
      case NFCMD_CLOSE :
      case NFCMD_SIZE :
      case NFCMD_TELL :
      case NFCMD_SEEK :
      case NFCMD_READ :
      {
    int fd;
    if (in->read(&fd,sizeof(fd))!=sizeof(fd)) comm_failed();

    switch (cmd)
    {
      case NFCMD_CLOSE :
      {
        fman->rf_close(fd);
        uint8_t st=1;
        if (out->write(&st,1)!=1) comm_failed();
      } break;
      case NFCMD_SIZE  :
      {
        int32_t x=fman->rf_file_size(fd);
        if (out->write(&x,sizeof(x))!=sizeof(x)) comm_failed();
      } break;
      case NFCMD_TELL :
      {
        int32_t offset=fman->rf_tell(fd);
        if (out->write(&offset,sizeof(offset))!=sizeof(offset)) comm_failed();
      } break;
      case NFCMD_SEEK :
      {
        int32_t offset;
        if (in->read(&offset,sizeof(offset))!=sizeof(offset)) comm_failed();
        offset=fman->rf_seek(fd,offset);
        if (out->write(&offset,sizeof(offset))!=sizeof(offset)) comm_failed();
      } break;
      case NFCMD_READ :
      {
        int32_t size;
        if (in->read(&size,sizeof(size))!=sizeof(size)) comm_failed();
        fman->rf_read(fd,out,size);
      } break;
    }
      } break;
      default :
      { fprintf(stderr,"net driver : unknown net command %d\n",cmd); die(0); }
    }
    ret=1;
  }

  ret|=game_face->process_net();
  return ret;
}


int net_driver::join_server(char *server_name)   // ask remote server for entry into game
{
  char sn_start[256];
  strcpy(sn_start,server_name);

  net_socket *sock=connect_to_server(server_name, DEFAULT_COMM_PORT, 0);
  if (!sock)
  {
    fprintf(stderr,"unable to connect\n");
    return 0;
  }

  uint8_t ctype=CLIENT_ABUSE;
  uint16_t port=lstl(game_port),cnum;

  uint8_t reg;
  if (sock->write(&ctype,1)!=1 ||   // send server out game port
      sock->read(&reg,1)!=1)        // is remote engine registered?
  { delete sock; return 0; }


  // maker sure the two games are both registered or unregistered or sync problems
  // will occur.

  if (!reg)
  {
    fprintf(stderr,
        "This server is not running the registered version of abuse, and\n"
        "you are (thanks!).  So that there are no conflict between the two games\n"
        "please start with the -share option when connecting to this server\n"
        "example : abuse -net somewhere.someplace.net -share\n");
    delete sock;
    return 0;
  }

  char uname[256];
  if (getlogin())
    strcpy(uname,getlogin());
  else strcpy(uname,"unknown");
  uint8_t len=strlen(uname)+1;

  if (sock->write(&len,1)!=1 ||
      sock->write(uname,len)!=len ||
      sock->write(&port,2)!=2  ||            // send server out game port
      sock->read(&port,2)!=2   ||            // read server's game port
      sock->read(&cnum,2)!=2   || cnum==0    // read player number (cannot be 0 because 0 is server)
      )
  { delete sock; return 0; }

  port=lstl(port);
  cnum=lstl(cnum);

  server_name=sn_start;
  net_socket *data_sock=connect_to_server(server_name,port,1,net_socket::SOCKET_FAST);
  if (!data_sock)  { delete sock; return 0; }

  delete game_face;
  game_face=new game_client(sn_start,sock,data_sock);
  return cnum;
}


net_socket *net_driver::connect_to_server(char *&name, int port, int force_port,
                      net_socket::socket_type sock_type)
{
  char *oname=name;
  net_address *addr=proto->get_node_address(name, port, force_port);
  if (!addr)
  {
    if (debug) fprintf(stderr,"No IP address for name %s\n",oname);
    return NULL;
  }

  if (debug)
    fprintf(stderr,"connecting to server %s\n",oname);
  net_socket *sock=proto->connect_to_server(addr,sock_type);
  delete addr;
  return sock;
}


int net_driver::get_lsf(char *name)  // contact remot host and ask for lisp startup file filename
{
  char *name_start=name;
  net_socket *sock=connect_to_server(name);
  if (!sock) return 0;

  uint8_t ctype=CLIENT_LSF_WAITER;
  uint8_t len;

  if (sock->write(&ctype,1)!=1 ||
      sock->read(&len,1)!=1 || len==0 ||
      sock->read(name_start,len)!=len)
  {
    delete sock;
    return 0;
  }

  delete sock;
  return 1;
}



int net_driver::fetch_crcs(char *server)
{
  net_socket *sock=connect_to_server(server);
  if (!sock) return 0;
  uint8_t cmd=CLIENT_CRC_WAITER;
  if (sock->write(&cmd,1)!=1 ||
      sock->read(&cmd,1)!=1)
  { delete sock; return 0; }
  delete sock;
  return cmd;
}


int net_driver::add_client(int type, net_socket *sock, net_address *from)
{
  switch (type)
  {
    case CLIENT_CRC_WAITER :
    {
      if (debug)
        fprintf(stderr,"add crc waiter\n");

      crc_wait_list=new crc_waiter(sock,crc_wait_list);
      base->calc_crcs=1;
      return 1;
    } break;
    case CLIENT_LSF_WAITER :
    {
      if (debug)
        fprintf(stderr,"add lsf waiter\n");
      lsf_wait_list=new lsf_waiter(sock,lsf_wait_list);
      base->get_lsf=1;
      return 1;
    } break;
    default :
    {
      int ret=game_face->add_client(type,sock,from);
      if (!ret && debug)
        fprintf(stderr,"unknown client type %d\n",type);
      return ret;
    }

  }
  return 0;
}

#endif // HAVE_NETWORK

