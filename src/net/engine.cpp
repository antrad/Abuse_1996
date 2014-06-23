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

#include "../inc/netface.h"      // net interface structures to the engine will use
#include "netfile.h"
#include "engine.h"

// these are the names of the fifos to create in tmp
// that communitcate with the engine

#define DIN_NAME "/tmp/.abuse_ndrv_in"
#define DOUT_NAME "/tmp/.abuse_ndrv_out"

// the lock files is used in case a previous net driver is already running

#define DLOCK_NAME "/tmp/.abuse_ndrv_lock"

#ifdef __sgi
#define next_process() sginap(0)
#else
#define next_process() usleep(10)
#endif


void net_watch();
void setup_ports(int comm_port, int game_port);

int no_security=0;
int driver_out_fd,driver_in_fd;
int shm_seg_id=-1;
void *shm_addr=(void *)-1;  // shmat returns -1 on failure
base_memory_struct *base;   // points to shm_addr
int comm_fd=-1,             // listening socket for commincation port
    game_fd=-1;
char net_server[256];   // if -net option, fetch all files from "active server"
int stand_alone=0;          // if we are running this stand-alone (not interfacing with the engine)

fd_set master_set;
fd_set master_write_set;    // set a socket here if you detect a write_full

join_struct *join_array;      // points to an array of possible joining clients in shared memory
int game_server_fd=-1,        // connection to server created by join_game()
    game_server_data_fd=-1;

int packet_port;              // port used to send 'lossy' game data

void clean_up()      // on exit unattach all shared memory links
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

  if (game_fd>0) close(game_fd);
  if (comm_fd>0) close(comm_fd);

  unlink(DIN_NAME);
  unlink(DOUT_NAME);
  unlink(DLOCK_NAME);
}

#ifdef __sgi
void die(...)
#else
void die(int why)
#endif
{
  fprintf(stderr,"dieing\n");
  clean_up();
  exit(0);
}


void mdie(char *reason)
{
  fprintf(stderr,"net driver : %s\n",reason);
  die(0);
}

void comm_failed()  // general communication failure with engine
{
  fprintf(stderr,"net driver : Error occurred while trying to communicate with the engine\n");
  clean_up();
  exit(0);
}


main(int argc, char **argv)
{
  int i;
  strcpy(default_fs_name,"");          // initially no default file server
  strcpy(net_server,"");

  for (i=1; i<argc; i++)
    if (!strcmp(argv[i],"-bastard"))   // this bypasses filename security features
    {
      fprintf(stderr,"Warning : Security measures bypassed (-bastard)\n");
      no_security=1;
    }


  // make sure this program was run by the abuse engine
  if (argc<2 || strcmp(argv[1],"runme"))
  {
    stand_alone=1;
    fprintf(stderr,"%s is normally run by abuse, running stand-alone file server\n"
               "Server will be killed by running abuse\n",argv[0]);
  }


  // see if we are already running, if so kill old driver
  FILE *fp=fopen(DLOCK_NAME,"rb");
  if (fp)
  {
    int pid;
    if (fscanf(fp,"%d",&pid)==1)
    {
      struct stat st;
      char proc_path[50];
      sprintf(proc_path,"/proc/%d",pid);
      if (!stat(proc_path,&st))
      {
    fprintf(stderr,"net driver : warning, %s already running, attempting to kill...\n",argv[0]);
    if (kill(pid,SIGKILL))
    {
      fprintf(stderr,"net driver : unable to kill process %d, cannot run net-abuse\n",pid);
      fclose(fp);
      return 0;
    }
    fprintf(stderr,"killed process %d\n",pid);
      }
    }
    fclose(fp);
    unlink(DLOCK_NAME);
  }


  unlink(DIN_NAME);    // remove any previous files if they existss
  unlink(DOUT_NAME);


  if (!stand_alone)
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

    int i,no_fork=0;
    for (i=1; i<argc; i++)
    if (!strcmp(argv[i],"-no_fork"))    // use this to debug easier
      no_fork=1;

    if (!no_fork)      // use this for debugging
    {
      int child_pid=fork();
      if (child_pid)
      {
    FILE *fp=fopen(DLOCK_NAME,"wb");
    if (!fp)
    {
      fprintf(stderr,"Unable to open %s for writing, killing child\n",DLOCK_NAME);
      kill(child_pid,SIGKILL);
      return 0;
    }
    fprintf(fp,"%d\n",child_pid);
    fclose(fp);
    printf("%d\n",child_pid);         // tell parent the sound driver's process number
    return 0;                         // exit, child will continue
      }
    }

    driver_out_fd=open(DOUT_NAME,O_RDWR);  // open the pipe
    if (driver_out_fd<0)
    { perror(DOUT_NAME);
      exit(1);
    }

    driver_in_fd=open(DIN_NAME,O_RDWR);
    if (driver_in_fd<0)
    { perror(DIN_NAME);
      exit(1);
    }
  } else driver_in_fd=driver_out_fd=-1;



  int catch_sigs[]={ SIGHUP,SIGINT,SIGQUIT,SIGILL,SIGABRT,
            SIGIOT,SIGFPE,SIGKILL,SIGUSR1,SIGSEGV,
            SIGUSR2,SIGPIPE,SIGTERM,SIGCHLD,
            SIGCONT,SIGSTOP,SIGTSTP,SIGTTIN,SIGTTOU,-1};

  for (i=0; catch_sigs[i]!=-1; i++)     // catch all signals in case we get
    signal(catch_sigs[i],die);            // interrupted before we remove shmid


/*  struct sigaction sa;
  memset(&sa,0,sizeof(sa));
  sa.sa_flags=SA_RESTART|SA_RESETHAND;
  sa.sa_handler=send_sig;
  sigaction(SIGUSR2,&sa,NULL); */


  int alloc_size=sizeof(join_struct)*MAX_JOINERS+
                 sizeof(base_memory_struct);

  shm_seg_id=shmget(IPC_PRIVATE,alloc_size,IPC_CREAT | 0777);
  if (shm_seg_id==-1)
    mdie("Unable to allocate shared memory");


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



  if (!stand_alone)
  {
    // see if we can attach this memory with the abuse engine
    if (write(driver_out_fd,&shm_seg_id,sizeof(shm_seg_id))!=sizeof(shm_seg_id))
      comm_failed();

    // wait for engine to ack it has attached
    uint8_t ack=0;
    if (read(driver_in_fd,&ack,1)!=1 || ack!=1)
      comm_failed();
  }


  if (shmctl(shm_seg_id,IPC_RMID,NULL))  // remove the shm id
    mdie("could not remove shm id");

  shm_seg_id=-1;                      // mark as not allocated


  int comm_port=DEFAULT_COMM_PORT;
  int game_port=-1;
  for (i=1; i<argc-1; i++)
    if (!strcmp(argv[i],"-port"))
    {
      comm_port=atoi(argv[i+1]);
      if (game_port==-1)
        game_port=comm_port+1;
    }
    else if (!strcmp(argv[i],"-game_port"))
      game_port=atoi(argv[i+1]);
    else if (!strcmp(argv[i],"-net"))
    { strcpy(net_server,argv[i+1]); }

  if (game_port==-1) game_port=DEFAULT_GAME_PORT+1;

  setup_ports(comm_port,game_port);


  net_watch();                        // now go into infinite block/read/process cycle

  return 0;
}

client *first_client=NULL;


void setup_ports(int comm_port, int game_port)
{
  sockaddr_in host;


  game_fd=socket(AF_INET,SOCK_DGRAM,0);
  if (game_fd==-1)
    mdie("net driver : could not create a socket.  (too many open files?)");

  int zz;
  if (setsockopt(game_fd,SOL_SOCKET,SO_REUSEADDR,(char *)&zz,sizeof(zz))<0)
    mdie("could not set socket option reuseaddr");

//  if (fcntl(game_fd,F_SETFL,FNDELAY)==-1)
//    mdie("cound not set udp socket to non-blocking");


  packet_port=game_port;   // save this port, so we can send it on joining a game
  memset( (char*) &host,0, sizeof(host));
  host.sin_family = AF_INET;
  host.sin_port = lstl(game_port);
  host.sin_addr.s_addr = lltl (INADDR_ANY);
  if (bind(game_fd, (struct sockaddr *) &host, sizeof(sockaddr_in))==-1)
  {
    fprintf(stderr,"net driver : could not bind socket to port %d\n",game_port);
    die(0);
  }


  // the comminication socket is a STREAM
  comm_fd=socket(AF_INET,SOCK_STREAM,0);
  if (comm_fd==-1)
    mdie("net driver : could not create a socket.  (too many open files?)");


  memset( (char*) &host,0, sizeof(host));
  host.sin_family = AF_INET;
  host.sin_port = lstl(comm_port);
  host.sin_addr.s_addr = lltl (INADDR_ANY);
  if (bind(comm_fd, (struct sockaddr *) &host, sizeof(sockaddr_in))==-1)
  {
    fprintf(stderr,"net driver : could not bind socket to port %d",comm_port);
    die(0);
  }

  if (listen(comm_fd,5)==-1)
  {
    fprintf(stderr,"net driver : could not listen to socket on port %d\n",comm_port);
    die(0);
  }




}


void delete_client(client *c)
{
  FD_CLR(c->socket_fd,&master_set);   // don't listen to this client anymore

}


inline int aquire_mem_lock()
{
  if (base->mem_lock==0 || base->mem_lock==1)
  {
    base->mem_lock=1;
    if (base->mem_lock==1)
      return 1;
  }
  return 0;
}

class crc_waiter
{
  public :
  int socket_fd;
  crc_waiter *next;
  crc_waiter(int fd, crc_waiter *Next)
  {
    FD_SET(fd,&master_set);   // set in case socket dies
    socket_fd=fd;
    next=Next;
  } ;
  ~crc_waiter()
  {
    close(socket_fd);
    FD_CLR(socket_fd,&master_set);
  }
} *crc_wait_list=NULL;

class lsf_waiter
{
  public :
  int socket_fd;
  lsf_waiter *next;
  lsf_waiter(int fd, lsf_waiter *Next)
  {
    FD_SET(fd,&master_set);   // set in case socket dies
    socket_fd=fd;
    next=Next;
  } ;
  ~lsf_waiter()
  {
    close(socket_fd);
    FD_CLR(socket_fd,&master_set);
  }
} *lsf_wait_list=NULL;


int connect_to_server(char *&server_name, int def_port, int stream_type, int force_port)
{
  char name[256],*np;
  np=name;
  while (*server_name && *server_name!=':' && *server_name!='/')
    *(np++)=*(server_name)++;
  *np=0;
  if (*server_name==':')
  {
    server_name++;
    char port[256],*p;
    p=port;
    while (*server_name && *server_name!='/')
      *(p++)=*(server_name++);
    *p=0;
    int x;
    if (!force_port)
    {
      if (sscanf(port,"%d",&x)==1) def_port=x;
      else return -1;
    }
  }

  if (*server_name=='/') server_name++;

  if (local_address(name))    // returns 1 if server name is ourself
  {
    fprintf(stderr,"cannot connect to %s, is a local address\n");
    return -1;
  }

  int socket_fd=socket(AF_INET,stream_type,0);
  if (socket_fd<0)
  {
    fprintf(stderr,"unable to create socket (too many open files?)\n");
    return -1;
  }


  hostent *hp=gethostbyname(name);
  if (!hp)
  {
    fprintf(stderr,"unable to locate server named '%s'\n",name);
    close(socket_fd);
    return 0;
  }


  sockaddr_in host;
  memset( (char*) &host,0, sizeof(host));
  host.sin_family = AF_INET;
  host.sin_port = lstl(def_port);
  host.sin_addr.s_addr = lltl (INADDR_ANY);
  memcpy(&host.sin_addr,hp->h_addr,hp->h_length);

  if (connect(socket_fd, (struct sockaddr *) &host, sizeof(host))==-1)
  {
    fprintf(stderr,"unable to connect\n");
    close(socket_fd);
    return -1;
  }

  return socket_fd;
}

int get_lsf(char *name)  // contact remot host and ask for lisp startup file filename
{
  char *name_start=name;
  int fd=connect_to_server(name);
  if (fd<0) return 0;
  uint8_t ctype=CLIENT_LSF_WAITER;
  if (write(fd,&ctype,1)!=1) { close(fd); return 0; }
  uint8_t len;
  if (read(fd,&len,1)!=1 || len==0) { close(fd); return 0; }
  if (read(fd,name_start,len)!=len) { close(fd); return 0; }
  close(fd);
  return 1;
}

int join_game(char *server_name)   // ask remote server for entry into game
{
  char sn_start[256];
  strcpy(sn_start,server_name);

  int fd=connect_to_server(server_name);
  uint8_t ctype=CLIENT_ABUSE;
  if (write(fd,&ctype,1)!=1) { close(fd); return 0; }

  // send server out game port
  uint16_t port=lstl(packet_port);
  if (write(fd,&port,2)!=2) { close(fd); return 0; }

  // read server's game port
  if (read(fd,&port,2)!=2) { close(fd); return 0; }
  port=lstl(port);

  uint16_t cnum;
  if (read(fd,&cnum,2)!=2 || cnum==0) { close(fd); return 0; }
  cnum=lstl(cnum);


  game_server_fd=fd;

  server_name=sn_start;
  game_server_data_fd=connect_to_server(server_name,port,SOCK_DGRAM,1);
  if (game_server_data_fd<0) mdie("could not udp-connect to server");
  FD_SET(game_server_fd,&master_set);

  return cnum;
}

void join_new_players()  // during this section we are giving mem_lock by engine
{
  client *c=first_client;

  for (; c; c=c->next)     // tell all the clients to reload
  {
    if (!c->has_joined)
    {
      uint16_t cnum=lstl(c->client_id);
      if (write(c->socket_fd,&cnum,2)!=2) { c->delete_me=1; }
      c->wait_reload=1;
      c->has_joined=1;
    } else if (!c->delete_me)
    {
      uint8_t reload=CLCMD_RELOAD;
      if (write(c->socket_fd,&reload,1)!=1) { c->delete_me=1; }
      c->wait_reload=1;
    }
  }

  base->join_list=NULL;  // all joiners have been added

}

int waiting_server_input=1;

void add_client_input(char *buf, int size, client *c)
{
  base->packet.add_to_packet(buf,size);

  if (c)
  {
    c->wait_input=0;
  }
  else
  {
    FD_SET(game_fd,&master_set);   // we are ready to accept other client's game data, so add the udp socket to the select list
    waiting_server_input=0;
  }

  int got_all=1;
  for (c=first_client; c; c=c->next)
    if (c->wait_input)
      got_all=0;

  if (got_all && !waiting_server_input)
  {
    base->packet.calc_checksum();

    for (c=first_client; c; c=c->next)      // setup for next time, wait for all the input
    {
      c->wait_input=1;
      send(c->data_fd,base->packet.data,base->packet.packet_size()+base->packet.packet_prefix_size(),0);
    }

    base->input_state=INPUT_PROCESSING; // tell engine to start processing
    FD_CLR(game_fd,&master_set);        // don't listen to this socket until we are prepared to read next tick's game data

    waiting_server_input=1;
  }
}

void get_input_from_server()
{
  int size=read(game_fd,base->packet.data,1024);
  if (size<=0)
  { mdie("read <= 0 bytes from server"); }
  if (base->packet.packet_size()+base->packet.packet_prefix_size()==size &&     // did we read the whole packet?
      base->packet.tick_received()==base->current_tick)    // if this was the correct tick packet, then tell server to go on
    base->input_state=INPUT_PROCESSING;
}

void process_engine_command()
{
  uint8_t cmd;
  if (read(driver_in_fd,&cmd,1)!=1) { mdie("could not read command from engine"); }
  switch (cmd)
  {
    case EGCMD_DIE :
    {
      if (!write(driver_out_fd,&cmd,1)) { mdie("could not write block ack1"); }  // send something to unblock engine
      mdie("received die command");
    } break;

    case NFCMD_BLOCK :
    {
      if (!write(driver_out_fd,&cmd,1)) { mdie("could not write block ack1"); }  // send something to unblock engine
      if (!read(driver_in_fd,&cmd,1)) { mdie("could not read block ack1"); }  // send something to block ourself
    } break;

    case NFCMD_INPUT_MISSING :    // try to fetch the input via a loss-less net protocol
    {
      unsigned char pk[2]={ CLCMD_REQUEST_RESEND,base->packet.tick_received()};

      if (net_server[0])   // if we are connected to a server ask sever to resend
      {
        if (write(game_server_fd,pk,2)!=2) { mdie("attept to re-fetch input failed"); }
    fprintf(stderr,"sending retry request to server (%d)\n",pk[1]);
      } else
      {
    client *c=first_client;
    for (; c; c=c->next)
    {
      if (!c->delete_me && c->wait_input)
      {
        fprintf(stderr,"sending retry request to client (%d)\n",pk[1]);
        if (write(c->socket_fd,pk,2)!=2)
          c->delete_me=1;
      }
      if (c->delete_me) fprintf(stderr,"delete this client!\n");
    }
      }
      if (!write(driver_out_fd,&cmd,1)) { mdie("could not write block ack1"); }  // send something to unblock engine
    } break;

    case NFCMD_SEND_INPUT :
    {
      base->packet.set_tick_received(base->current_tick);
      base->input_state=INPUT_COLLECTING;
      if (!net_server[0])
      {
        add_client_input(NULL,0,NULL);
      }
      else
      {
    base->packet.calc_checksum();
        send(game_server_data_fd,base->packet.data,base->packet.packet_size()+base->packet.packet_prefix_size(),0);
      }
      if (!write(driver_out_fd,&cmd,1)) { mdie("could not write send ack1"); }  // send something to unblock engine
      if (!read(driver_in_fd,&cmd,1)) { mdie("could not read send ack2"); }    // read something to block ourselves for engine
    } break;

    case NFCMD_RELOADED :
    {
      if (game_server_fd>0)
      {
    uint8_t ok=CLCMD_RELOADED;
        if (!write(game_server_fd,&ok,1)) { mdie("could not send join_ok msg"); }
    next_process();
      }
    } break;

    case NFCMD_PROCESS_ENTRIES :  // server is telling us the player has been added into the game
    {
      join_new_players();
    } break;

    case NFCMD_REQUEST_ENTRY :
    {
      uint8_t len;
      char name[256];
      if (read(driver_in_fd,&len,1)!=1) { mdie("could not read server name length"); }
      if (read(driver_in_fd,name,len)!=len) { mdie("could not read server name"); }
      strcpy(net_server,name);
      uint16_t success=join_game(name);
      if (write(driver_out_fd,&success,2)!=2) mdie("cound not send lsf read failure");
      next_process();
    } break;

    case NFCMD_REQUEST_LSF :
    {
      uint8_t len;
      char name[256];
      if (read(driver_in_fd,&len,1)!=1) { mdie("could not read lsf name length"); }
      if (read(driver_in_fd,name,len)!=len) { mdie("could not read lsf name"); }
      if (!get_lsf(name))
      {
    len=0;
        if (write(driver_out_fd,&len,1)!=1) mdie("cound not send lsf read failure");
      } else
      {
    len=strlen(name)+1;
    if (write(driver_out_fd,&len,1)!=1) mdie("cound not send lsf name len");
    if (write(driver_out_fd,name,len)!=len) mdie("cound not send lsf name");
      }
      next_process();
    } break;

    case NFCMD_PROCESS_LSF :
    {
      uint8_t len,name[256];
      if (read(driver_in_fd,&len,1)!=1) { mdie("could not read lsf name length"); }
      if (read(driver_in_fd,name,len)!=len) { mdie("could not read lsf name"); }
      while (lsf_wait_list)
      {
    lsf_waiter *c=lsf_wait_list;
    lsf_wait_list=lsf_wait_list->next;
    uint8_t status=1;
    write(c->socket_fd,&len,1);
    write(c->socket_fd,name,len);
    delete c;
      }
      next_process();
    } break;

    case NFCMD_CRCS_CALCED :
    {
      while (crc_wait_list)
      {
    crc_waiter *c=crc_wait_list;
    crc_wait_list=crc_wait_list->next;
    uint8_t status=1;
    write(c->socket_fd,&status,1);
    delete c;
      }
      next_process();
    } break;

    case NFCMD_SET_FS :
    {
      uint8_t size;
      char sn[256];
      if (read(driver_in_fd,&size,1)!=1) mdie("could not read filename length");
      if (read(driver_in_fd,sn,size)!=size) mdie("could not read server name");
      strcpy(default_fs_name,sn);
      size=fetch_crcs(sn);  // return success
      if (write(driver_out_fd,&size,1)!=1) mdie("could not send ok to engine");
      next_process();
    } break;

    case NFCMD_OPEN :
    {
      uint8_t size[2];
      char filename[300],mode[20],*fn;
      fn=filename;
      if (read(driver_in_fd,size,2)!=2) mdie("could not read fd on open");
      if (read(driver_in_fd,filename,size[0])!=size[0]) mdie("incomplete filename");
      if (read(driver_in_fd,mode,size[1])!=size[1]) mdie("incomplete mode string");

      int fd=open_file(fn,mode);
      if (fd==-2)
      {
    uint8_t st[2];
    st[0]=NF_OPEN_LOCAL_FILE;
    st[1]=strlen(fn)+1;
    if (write(driver_out_fd,st,2)!=2) comm_failed();
    int size=write(driver_out_fd,fn,st[1]);
    if (size!=st[1]) comm_failed();

    if (size!=st[1]) comm_failed();
      } else if (fd==-1)
      {
    uint8_t st=NF_OPEN_FAILED;
    if (write(driver_out_fd,&st,1)!=1) comm_failed();
      } else
      {
    uint8_t st=NF_OPEN_REMOTE_FILE;
    if (write(driver_out_fd,&st,1)!=1) comm_failed();
    if (write(driver_out_fd,&fd,sizeof(fd))!=sizeof(fd)) comm_failed();
      }
      next_process();
    } break;
    case NFCMD_CLOSE :
    case NFCMD_SIZE :
    case NFCMD_TELL :
    case NFCMD_SEEK :
    case NFCMD_READ :
    {
      int fd;
      if (read(driver_in_fd,&fd,sizeof(fd))!=sizeof(fd)) comm_failed();
      remote_file *rf=find_rfile(fd);
      if (!rf)
    mdie("bad fd for engine command");

      switch (cmd)
      {
    case NFCMD_CLOSE :
    {
      unlink_remote_file(rf);
      delete rf;
      uint8_t st=1;
      if (write(driver_out_fd,&st,1)!=1) comm_failed();
    } break;
    case NFCMD_SIZE  :
    {
      if (write(driver_out_fd,&rf->size,sizeof(rf->size))!=sizeof(rf->size)) comm_failed();
    } break;
    case NFCMD_TELL :
    {
      long offset=rf->unbuffered_tell();
      if (write(driver_out_fd,&offset,sizeof(offset))!=sizeof(offset)) comm_failed();
    } break;
    case NFCMD_SEEK :
    {
      long offset;
      if (read(driver_in_fd,&offset,sizeof(offset))!=sizeof(offset)) comm_failed();
      offset=rf->unbuffered_seek(offset);
      if (write(driver_out_fd,&offset,sizeof(offset))!=sizeof(offset)) comm_failed();
    } break;
    case NFCMD_READ :
    {
      long size;
      if (read(driver_in_fd,&size,sizeof(size))!=sizeof(size)) comm_failed();
      rf->unbuffered_read(driver_out_fd,size);
    } break;
      }
      next_process();
    } break;
    default :
    { fprintf(stderr,"net driver : unknown net command %d\n",cmd); die(0); }
  }
}


int process_client_command(client *c)
{
  uint8_t cmd;
  if (read(c->socket_fd,&cmd,1)!=1) return 0;
  switch (cmd)
  {
    case CLCMD_RELOADED :
    {
      c->wait_reload=0;
      int done=1;
      for (c=first_client; c; c=c->next)
        if (c->wait_reload) done=0;
      if (done) base->wait_reload=0;
      return 1;
    } break;
    case CLCMD_REQUEST_RESEND :
    {
      uint8_t tick;
      if (read(c->socket_fd,&tick,1)!=1) return 0;


      fprintf(stderr,"request for resend tick %d (game cur=%d, pack=%d, last=%d)\n",
          tick,base->current_tick,base->packet.tick_received(),base->last_packet.tick_received());

      if (tick==base->last_packet.tick_received())
      {
    fprintf(stderr,"resending last game packet\n");
    send(c->data_fd,base->last_packet.data,base->last_packet.packet_size()+base->last_packet.packet_prefix_size(),0);
      }
      else if (tick==base->packet.tick_received()) // asking for current tick, make sure it's collected
      {
    int got_all=!waiting_server_input;
    client *cc=first_client;
    for (; cc; cc=cc->next)
      if (cc->wait_input) got_all=0;

    if (got_all)
    {
      fprintf(stderr,"resending current game packet\n");
      send(c->data_fd,base->packet.data,base->packet.packet_size()+base->packet.packet_prefix_size(),0);
    } else fprintf(stderr,"current game packet not complete yet\n");
      }
      return 1;
    } break;
  }
  return 0;
}



int isa_client(int client_id)    // sreach the list of active clients for this id and return 1 if found
{
  int i;
  if (client_id==0) return 1;   // the server is always a client
  client *c=first_client;
  for (; c; c=c->next)
    if (c->client_id==client_id) return 1;
  return 0;
}

int join_game_client(int client_id)
{
}

int add_game_client(int fd, sockaddr *from)     // returns false if could not join client
{
  uint16_t port;
  if (read(fd,&port,2)!=2) { close(fd);  return 0; }
  port=lstl(port);

  uint16_t pport=lstl(packet_port);
  if (write(fd,&pport,2)!=2) { close(fd);  return 0; }


  int f=-1,i;
  for (i=0; f==-1 && i<MAX_JOINERS; i++)
    if (!isa_client(i))
      f=i;

  if (f===-1) { close(fd); return 0; }

  uint16_t client_id=lstl(f);
  if (write(fd,&client_id,2)!=2) { close(fd);  return 0; }


  join_array[f].next=base->join_list;
  base->join_list=real2shm(join_struct,&join_array[f]);
  join_array[f].client_id=first_free_client;

  first_client=new client(fd,f,first_client);
  memcpy(&first_client->data_address,from,sizeof(first_client->data_address));

  // data port should be one above comminication port
  first_client->data_address.sin_port = lstl(port);

  first_client->data_fd=socket(AF_INET,SOCK_DGRAM,0);

  if (first_client->data_fd==-1)
  {
    client *c=first_client;  first_client=first_client->next; delete c;
    fprintf(stderr,"net driver : could not create a socket.  (too many open files?)");
    return 0;
  }

  if (connect(first_client->data_fd, (struct sockaddr *) &first_client->data_address,
          sizeof(first_client->data_address))==-1)
  {
    client *c=first_client;  first_client=first_client->next; delete c;
    fprintf(stderr,"unable to connect upd port\n");
    return 0;
  }


  return 1;
}


void add_client()
{
  struct sockaddr from;
  int addr_len=sizeof(from);
  int new_fd=accept(comm_fd,&from,&addr_len);
  if (new_fd>=0)
  {
    char client_type;
    if (read(new_fd,&client_type,1)!=1) { close(new_fd); return ; }
    switch (client_type)
    {
      case CLIENT_NFS : add_nfs_client(new_fd);    break;
      case CLIENT_ABUSE : add_game_client(new_fd,&from); break;
      case CLIENT_CRC_WAITER :
      {
    if (stand_alone)    // can't ask the engine if there is no engine
    {
      char status=0;
      write(new_fd,&status,1);
      close(new_fd);
    } else
    {
      crc_wait_list=new crc_waiter(new_fd,crc_wait_list);
      base->calc_crcs=1;
    }
      } break;
      case CLIENT_LSF_WAITER :
      {
    if (stand_alone)    // can't ask the engine if there is no engine
    {
      char status=0;
      write(new_fd,&status,1);
      close(new_fd);
    } else
    {
      lsf_wait_list=new lsf_waiter(new_fd,lsf_wait_list);
      base->get_lsf=1;
    }
      } break;
    }
  }
}

void net_watch()
{
  int i;
  join_array=(join_struct *) (base+1);

  for (i=0; i<MAX_JOINERS; i++)
    join_array[i].client_id=-1;


  if (!stand_alone)
  {
    while (!aquire_mem_lock()) { next_process(); }
    base->mem_lock=0;
  }

  fd_set read_set,exception_set,write_set;

  FD_ZERO(&master_set);
  FD_ZERO(&master_write_set);
  FD_SET(comm_fd,&master_set);     // new incoming connections & nfs data
  if (net_server)
    FD_SET(game_fd,&master_set);     // new incoming connections & nfs data

  if (!stand_alone)
  {
    FD_SET(driver_in_fd,&master_set);  // request from engine
    FD_SET(driver_out_fd,&master_set); // check for error on messages to engine
  }

  while (1)
  {
    memcpy(&read_set,&master_set,sizeof(master_set));
    memcpy(&exception_set,&master_set,sizeof(master_set));
    memcpy(&write_set,&master_write_set,sizeof(master_set));


    int tsel=select(FD_SETSIZE,&read_set,&write_set,&exception_set,NULL);

    int check_rest=1;
    if (!stand_alone)
    {
      if (base->input_state==INPUT_COLLECTING)
      {
    // any game related data (udp) waiting to be read?
    if (FD_ISSET(game_fd,&read_set))
    {
      tsel--;
      check_rest=0;
      net_packet scratch,*use;

      if (net_server[0]==0)    // if we are the server, read into scratch, then "add" into base
        use=&scratch;
      else use=&base->packet;    // otherwise read directly into base because it is a complete packet from the server

      sockaddr_in from_addr;
      int addr_size=sizeof(from_addr);
      int bytes_received=recvfrom(game_fd,use->data,1024,0, (sockaddr *)&from_addr,&addr_size);

      // make sur we got a complete packet and the packet was not a previous game tick packet
      if (bytes_received==use->packet_size()+use->packet_prefix_size())
      {
        unsigned short rec_crc=use->get_checksum();
        use->calc_checksum();
        if (rec_crc==use->get_checksum())
        {
          if (base->current_tick==use->tick_received())
          {
        if (net_server[0])   // if we are a client, tell game to process input
        base->input_state=INPUT_PROCESSING;   // tell engine to start processing
        else
        {

          client *f=first_client,*found=NULL;
          for (; !found &&f; f=f->next)
          if (!memcmp(&from_addr.sin_addr,&f->data_address.sin_addr,sizeof(from_addr.sin_addr)))
          found=f;

          if (!found)
          fprintf(stderr,"received data from unknown client\n");
          else
          add_client_input((char *)use->packet_data(),use->packet_size(),found);
        }
          } else fprintf(stderr,"received stale packet (got %d, expected %d)\n",use->tick_received(),base->current_tick);
        } else fprintf(stderr,"received packet with bad checksum\n");
      } else fprintf(stderr,"received incomplete packet\n");
    }
    base->mem_lock=0;
      }

      // see if we had any errors talking to the engine
      if (FD_ISSET(driver_in_fd,&exception_set) || FD_ISSET(driver_out_fd,&exception_set))
      {
    tsel--;
    check_rest=0;
        comm_failed();
      }

      // see if the engine has anything to say before getting to anyone else
      if (FD_ISSET(driver_in_fd,&read_set))
      {
    tsel--;
        process_engine_command();
    check_rest=0;
      }
    }


    if (check_rest && aquire_mem_lock())  // we need to change shared memory, make sure server is not using it.
    {
      if (game_server_fd==-1)    // we are a server, check all client connections
      {
    client *c,*lastc=NULL;

    for (c=first_client; c; )
    {
      int del=0;
      if (FD_ISSET(c->socket_fd,&exception_set))  // error?
      {
        tsel--;
        del=1;
      }

      // waiting for engine to process command buffer, don't read anymore yet
      else if (FD_ISSET(c->socket_fd,&read_set))  // in comming commands data from client?
      {
        tsel--;
        if (process_client_command(c)==0)
        del=1;

        if (del)
        {
          if (c->wait_reload)
          {
        int done=1;
        client *d=first_client;
        for (; d; d=d->next)                // see if this was the last client to wait on reloading
        if (d->wait_reload) done=0;
        if (done) base->wait_reload=0;
          }

          if (lastc) lastc->next=c->next;
          else first_client=c->next;
          client *cd=c; c=c->next; delete cd;
        } else
        {
          lastc=c;
          c=c->next;
        }
      } else c=c->next;
    }
      } else if (FD_ISSET(game_server_fd,&read_set))
      {
    uint8_t cmd;
    if (read(game_server_fd,&cmd,1)!=1) { mdie("unable to read command from server"); }
    switch (cmd)
    {
      case CLCMD_RELOAD :
      {
        base->need_reload=1;
      } break;
      case CLCMD_REQUEST_RESEND :
      {
        uint8_t tick;
        if (read(game_server_fd,&tick,1)!=1) { mdie("unable to read resend tick from server"); }

        fprintf(stderr,"request for resend tick %d (game cur=%d, pack=%d, last=%d)\n",
          tick,base->current_tick,base->packet.tick_received(),base->last_packet.tick_received());

        if (tick==base->packet.tick_received() && !waiting_server_input)    // asking for this tick?  make sure is collected
        {
          fprintf(stderr,"resending client packet to server\n");
          send(game_server_data_fd,base->packet.data,base->packet.packet_size()+base->packet.packet_prefix_size(),0);
        }
      } break;
    }
      }



      if (FD_ISSET(comm_fd,&read_set))
      {
    tsel--;
        add_client();
      }

      nfs_client *nc,*last=NULL;
      for (nc=first_nfs_client; nc; )      // check for nfs request
      {

    int ok=1;

    if (FD_ISSET(nc->socket_fd,&exception_set))
    {
      tsel--;
      ok=0;
      fprintf(stderr,"Killing nfs client, socket went bad\n");
    }
    else if (nc->size_to_read)
    {
      if (FD_ISSET(nc->socket_fd,&write_set))
      {
        tsel--;
        ok=nc->send_read();
      }
    }
    else if (FD_ISSET(nc->socket_fd,&read_set))
    {
      tsel--;
      ok=process_nfs_command(nc);    // if we couldn't process the packeted, delete the connection
    }

    if (ok)
    {
      last=nc;
      nc=nc->next;
    } else
    {
      if (last) last->next=nc->next;
      else first_nfs_client=nc->next;
      nfs_client *c=nc;
      nc=nc->next;
      delete c;
    }
      }

      // check for bad sockets for people waiting on crc's
      crc_waiter *crcw=crc_wait_list,*last_crcw=NULL;
      for (; crcw; )
      {
    if (FD_ISSET(crcw->socket_fd,&exception_set))
    {
      tsel--;
      if (last_crcw) { last_crcw->next=crcw->next; crc_waiter *cc=crcw; crcw=crcw->next; delete cc; }
      else { crc_wait_list=crcw->next; delete crcw; crcw=crc_wait_list; }
    } else crcw=crcw->next;
      }
      if (!crc_wait_list) base->calc_crcs=0;

      // check for bad sockets for people waiting on crc's
      lsf_waiter *lsfw=lsf_wait_list,*last_lsfw=NULL;
      for (; lsfw; )
      {
    if (FD_ISSET(lsfw->socket_fd,&exception_set))
    {
      tsel--;
      if (last_lsfw) { last_lsfw->next=lsfw->next; lsf_waiter *cc=lsfw; lsfw=lsfw->next; delete cc; }
      else { lsf_wait_list=lsfw->next; delete lsfw; lsfw=lsf_wait_list; }
    } else lsfw=lsfw->next;
      }
      if (!lsf_wait_list) base->get_lsf=0;

      base->mem_lock=0;

    }
    if (tsel)
    {
//      fprintf(stderr,"%d",tsel);
      next_process();
    }

  }
}

#endif // HAVE_NETWORK

