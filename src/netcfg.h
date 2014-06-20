/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __NETCFG_HPP_
#define __NETCFG_HPP_

#include "jwindow.h"

class net_configuration
{
  public :
  enum { SINGLE_PLAYER, SERVER, CLIENT, RESTART_SERVER, RESTART_CLIENT, RESTART_SINGLE } state;

  int restart_state();
  int notify_reset();

  unsigned short port,
                 server_port;  // if we are a server, use our_port
  char name[100];
  char server_name[100];


  char min_players,
       max_players;
  short kills;

  net_configuration();
  int input();   // pulls up dialog box and input fileds
  void cfg_error(char const *msg);
  int confirm_inputs(Jwindow *j, int server);
  void error(char const *message);
  int confirm_inputs(InputManager *i, int server);
  ifield *center_ifield(ifield *i,int x1, int x2, ifield *place_below);
  int get_options(int server);
} ;

extern net_configuration *main_net_cfg;

#endif
