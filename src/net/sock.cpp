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

#include <stdlib.h>

#include "sock.h"

const char notify_signature[] = "I wanna play ABUSE!";
const char notify_response[] = "Yes!";

net_protocol *net_protocol::first=0;

#if HAVE_NETWORK
// connect to an explictedly named address
// first try to get the address and then try to connect
// return NULL if either fail.  This method does not need to be implemented
// in sub-classes
net_socket *net_protocol::connect_to_server(char const *&server_name, int port, int force_port, net_socket::socket_type sock_type)
{
  net_address *a=get_node_address(server_name,port,force_port);
  if (!a) return NULL;
  net_socket *s=connect_to_server(a,sock_type);
  delete a;
  return s;
}
#endif

