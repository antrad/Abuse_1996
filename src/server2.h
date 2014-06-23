/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __SERVER2_HPP_
#define __SERVER2_HPP_
#error hi

class client_descriptor;


class game_server
{
  client_descriptor *client_list;
  int sync_check;
  public :
  game_server(int argc, char **argv, int port);
//  int init_failed() { return in==NULL; }
  void check_for_clients();
  void receive_inputs();         // reads inputs from all non-local clients
  void send_inputs();            // pass collected inputs to all non-local clients
  void join_new_players();
  ~game_server();
} ;

extern int start_running;
extern game_server *local_server;       // created on server machine, NULL on all others

void game_net_init(int argc, char **argv);
#endif


