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
#include <netdb.h>

#include "common.h"

#include "undrv.h"
#include "../include/netface.h" // net interface structures the engine will use

//#include "netdrv.h"
#include "gserver.h"
#include "gclient.h"
#include "fileman.h"
#include "sock.h"
#include "tcpip.h"



static int comm_fd=-1,game_fd=-1;
net_socket *game_sock=NULL,*comm_sock=NULL;

void undrv_cleanup()
{
  if (game_sock) { delete game_sock; game_sock=NULL; }
  if (comm_sock) { delete comm_sock; comm_sock=NULL; }
}


void setup_ports(int comm_port, int game_port)
{
  comm_sock=tcpip.create_listen_socket(comm_port,net_socket::SOCKET_SECURE);
  if (!comm_sock) mdie("net driver : could not setup comminication socket");
  comm_sock->read_selectable();

  game_sock=tcpip.create_listen_socket(game_port,net_socket::SOCKET_FAST);
  if (!game_sock) mdie("net driver : could not setup data socket");
  game_sock->read_selectable();
}

int kill_old_driver(int argc, char **argv)
{
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
  return 1;
}

main(int argc, char **argv)
{
  if (!kill_old_driver(argc,argv))
    return 0;

  int no_fork=0,i;
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
    kill(child_pid,SIGUSR2);
    return 0;
      }
      fprintf(fp,"%d\n",child_pid);
      fclose(fp);
      printf("%d\n",child_pid);         // tell parent the sound driver's process number
      return 0;                         // exit, child will continue
    }
  }

  fman=new file_manager(argc,argv,&tcpip);

  int comm_port=DEFAULT_COMM_PORT;
  int game_port=-1;
  int debug=0;

  for (i=1; i<argc-1; i++)
    if (!strcmp(argv[i],"-port"))
    {
      comm_port=atoi(argv[i+1]);
      if (game_port==-1)
        game_port=comm_port+1;
    }
    else if (!strcmp(argv[i],"-game_port"))
      game_port=atoi(argv[i+1]);
    else if (!strcmp(argv[i],"-debug"))
      debug=1;


  if (game_port==-1) game_port=DEFAULT_GAME_PORT;

  // make sure this program was run by the abuse engine
  if (argc<2 || strcmp(argv[1],"runme"))
  {
    fprintf(stderr,"%s is normally run by abuse, running stand-alone file server\n"
               "Server will be killed by running abuse\n",argv[0]);
  } else driver=new net_driver(argc,argv,comm_port,game_port,&tcpip);

  setup_ports(comm_port,game_port);

  while (1)
  {
    tcpip.select_sockets();
    if (driver)
      driver->check_commands();

    if (comm_sock->ready_to_read())
    {
      net_address *addr;

      net_socket *new_sock=comm_sock->accept(addr);
      if (debug)
      {
    if (new_sock)
    {
      fprintf(stderr,"accepting new connection from \n");
      addr->print();
    } else
    fprintf(stderr,"accept failed\n");
      }


      if (new_sock)
      {
    uchar client_type;
    if (new_sock->read(&client_type,1)!=1)
    {
      delete addr;
      delete new_sock;
    }
    else if (client_type==CLIENT_NFS)
    {
      delete addr;
      fman->add_nfs_client(new_sock);
    }
    else if (!driver || !driver->add_client(client_type,new_sock,addr))
    {
      delete addr;
      delete new_sock;
    }
      }
    }

    fman->process_net();
  }
}

#endif // HAVE_NETWORK

