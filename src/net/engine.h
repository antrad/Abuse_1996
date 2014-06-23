// engine.c is the what comminicates with the abuse engine

#ifndef __ENGINE_HPP_
#define __ENGINE_HPP_

#include "../inc/netface.h"
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


#define DEFAULT_COMM_PORT 20202
#define DEFAULT_GAME_PORT 20203
#define MAX_JOINERS 32  // maximum clients that can join at the same time


extern int no_security;
extern int driver_out_fd,driver_in_fd;
struct base_memory_struct;
extern base_memory_struct *base;


extern fd_set master_set;        // list of sockets to block for
extern fd_set master_write_set;  // set a socket here if you detect a write_full


void comm_failed();              // call if problems talking to engine
void mdie(char *reason);         // call if general net problems


#define real2shm(type,ptr) (ptr==NULL ? NULL : ((type *)((char *)(ptr)-(char *)base)))
#define shm2real(type,ptr) (ptr==NULL ? NULL : ((type *)((long)(ptr)+(long)(base))))

class client
{
public :
  int socket_fd;
  int data_fd;
  int client_id;       // index into client_struct
  int has_joined;
  int wait_reload;
  int wait_input;
  int delete_me;
  struct sockaddr_in data_address;  // where to send game data

  client *next;
  client(int sock, int id, client *Next)
  {
    data_fd=-1;
    socket_fd=sock;
    client_id=id;
    next=Next;
    has_joined=0;
    delete_me=0;
    wait_reload=0;
    wait_input=1;
    FD_SET(socket_fd,&master_set);   // set in case socket dies
  }
  ~client()
  {
    close(socket_fd);
    FD_CLR(socket_fd,&master_set);
    if (data_fd>0)
      close(data_fd);
  } ;
} ;

extern client *first_client;    // list of clients
int connect_to_server(char *&server_name, int def_port=DEFAULT_COMM_PORT, int stream_type=SOCK_STREAM, int force_port=0);  // -1 on failure

#endif



