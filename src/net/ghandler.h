#ifndef __GHANDLER_HPP_
#define __GHANDLER_HPP_

#include "netface.h"


#define DEFAULT_COMM_PORT 20202
#define DEFAULT_GAME_PORT 20203


extern base_memory_struct *base;

class game_handler     // game_client and game_serevr are derived from here
{
  public :
  virtual int process_net()      { return 1; }     // return 0 if net-shutdown need to happen
  virtual void add_engine_input() { base->input_state=INPUT_PROCESSING; }
  virtual int input_missing()    { return 1; }  // request input re-send  ( return 0 if net-shutdown needs to happen)
  virtual int start_reload()      { return 1; }
  virtual int end_reload(int disconenct=0) { return 1; }
  virtual int add_client(int type, net_socket *sock, net_address *from) { return 0; }
  virtual int kill_slackers()     { return 1; }
  virtual int quit()              { return 1; }  // should disconnect from everone and close all sockets
  virtual void game_start_wait()  { ; }
  virtual ~game_handler()         { ; }
} ;

#endif


