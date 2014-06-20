#ifndef __GCLIENT_HPP_
#define __GCLIENT_HPP_

#include <unistd.h>
#include "sock.h"
#include "ghandler.h"

class game_client : public game_handler
{
  net_socket *client_sock;             // connection to server as a client
  int wait_local_input;
  int process_server_command();
  net_address *server_data_port;
  public :

  game_client(net_socket *client_sock, net_address *server_addr);
  int process_net();
  int input_missing();
  void add_engine_input();
  virtual int start_reload();
  virtual int end_reload(int disconnect=0);
  virtual int kill_slackers();
  virtual int quit();
  virtual ~game_client();
} ;


#endif
