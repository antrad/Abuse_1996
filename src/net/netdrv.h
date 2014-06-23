#ifndef __NETDRV_HPP_
#define __NETDRV_HPP_

#error hi
#include "netface.h"
#include <unistd.h>
#include "undrv.h"
// these are the names of the fifos to create in tmp
// that communitcate with the engine

#define DIN_NAME "/tmp/.abuse_ndrv_in"
#define DOUT_NAME "/tmp/.abuse_ndrv_out"

// the lock files is used in case a previous net driver is already running

#define DLOCK_NAME "/tmp/.abuse_ndrv_lock"
#include "sock.h"


class game_handler;

class net_driver
{
  net_socket *in,*out;
  int shm_seg_id;
  void *shm_addr;

  int connect_to_engine(int argc, char **argv);
  int setup_shm();
  game_handler *game_face;
  int join_server(char *server_name);
  net_protocol *proto;
  int comm_port, game_port;
  join_struct *join_array;
  int debug;
  int reg;

  class lsf_waiter
  {
    public :
    net_socket *sock;
    lsf_waiter *next;
    lsf_waiter(net_socket *sock, lsf_waiter *next) : sock(sock), next(next)
    {
      sock->read_selectable();  // set in case socket dies
    } ;
    ~lsf_waiter()
    {
      sock->read_unselectable();
      delete sock;
    }
  } *lsf_wait_list;


  class crc_waiter
  {
    public :
    net_socket *sock;
    crc_waiter *next;
    crc_waiter(net_socket *sock, crc_waiter *next) : sock(sock), next(next)
    {
      sock->read_selectable();  // set in case socket dies
    } ;
    ~crc_waiter()
    {
      sock->read_unselectable();
      delete sock;
    }
  } *crc_wait_list;

  int fetch_crcs(char *server);
  int get_lsf(char *name);  // contact remot host and ask for lisp startup file filename

  public :
  base_memory_struct *base;

  void cleanup();
  int registered() { return reg; }
  net_driver(int argc, char **argv, int comm_port, int game_port, net_protocol *proto);
  int check_commands();
  int add_client(int type, net_socket *sock, net_address *from);
  int add_joiner(int client_id, char *name);
  net_socket *connect_to_server(char *&name,
                int port=DEFAULT_COMM_PORT,
                int force_port=0,
                net_socket::socket_type sock_type=net_socket::SOCKET_SECURE);
  net_protocol *get_protocol() { return proto; }
  int become_server();
  int get_game_port() { return game_port; }
  ~net_driver();
} ;

extern net_driver *driver;
void mdie(char *reason);

#endif

