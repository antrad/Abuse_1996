#ifndef __UNDRV_HPP_
#define __UNDRV_HPP_

#define DEFAULT_COMM_PORT 20202
#define DEFAULT_GAME_PORT 20203
#define MAX_JOINERS 32  // maximum clients that can join at the same time

#include "sock.h"

void undrv_cleanup();

extern net_socket *game_sock,*comm_sock;

#endif

