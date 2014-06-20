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

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <signal.h>

#include "common.h"

#include "netcfg.h"
#include "gclient.h"
#include "netface.h"
#include "undrv.h"
#include "timing.h"

extern base_memory_struct *base;
extern net_socket *comm_sock,*game_sock;
extern net_protocol *prot;
extern char lsf[256];
extern int start_running;

int game_client::process_server_command()
{
  uint8_t cmd;
  if (client_sock->read(&cmd,1)!=1) return 0;
  switch (cmd)
  {
    case CLCMD_REQUEST_RESEND :
    {
      uint8_t tick;
      if (client_sock->read(&tick,1)!=1) return 0;

      fprintf(stderr,"request for resend tick %d (game cur=%d, pack=%d, last=%d)\n",
          tick,base->current_tick,base->packet.tick_received(),base->last_packet.tick_received());

      // asking for this tick?  make sure it is collected
      if (tick==base->packet.tick_received() && !wait_local_input)
      {
    fprintf(stderr,"resending client packet %d to server\n",base->packet.tick_received());
    net_packet *pack=&base->packet;
    game_sock->write(pack->data,pack->packet_size()+pack->packet_prefix_size(),server_data_port);

    { time_marker now,start; while (now.diff_time(&start)<3.0) now.get_time(); }
      }
      return 1;
    } break;
    default :
    {
      fprintf(stderr,"unknown command from server %d\n",cmd);
      return 0;
    }
  }
  return 0;
}

int game_client::process_net()
{
  if (client_sock->error())
    return 0;

  if (game_sock->ready_to_read())     // any game data comming in?
  {
    net_packet tmp;  // don't store in main packet in case something is wrong with this packet and server still needs old one

    int bytes_received=game_sock->read(tmp.data,PACKET_MAX_SIZE);
    if (bytes_received==tmp.packet_size()+tmp.packet_prefix_size())   // was the packet complete?
    {
      uint16_t rec_crc=tmp.get_checksum();
      if (rec_crc==tmp.calc_checksum())
      {
    if (base->current_tick==tmp.tick_received())
    {
      base->packet=tmp;
      wait_local_input=1;
      base->input_state=INPUT_PROCESSING;   // tell engine to start processing
    }
//    else fprintf(stderr,"received stale packet (got %d, expected %d)\n",tmp.tick_received(),base->current_tick);
      } else fprintf(stderr,"received packet with bad checksum\n");
    } else fprintf(stderr,"incomplete packet, read %d, should be %d\n",bytes_received,tmp.packet_size()+tmp.packet_prefix_size());

  }

  if (client_sock->ready_to_read())
  {
    if (!process_server_command())
    {
      main_net_cfg->state=net_configuration::RESTART_SINGLE;
      start_running=0;
      strcpy(lsf,"abuse.lsp");

      wait_local_input=1;
      base->input_state=INPUT_PROCESSING;   // tell engine to start processing
      return 0;

    }
  }
  return 1;
}


game_client::game_client(net_socket *client_sock, net_address *server_addr) :
  client_sock(client_sock)
{
 server_data_port=server_addr->copy();
  client_sock->read_selectable();
  wait_local_input=1;
}

int game_client::input_missing()
{
  if (prot->debug_level(net_protocol::DB_IMPORTANT_EVENT))
    fprintf(stderr,"(resending %d)\n",base->packet.tick_received());
  net_packet *pack=&base->packet;
  game_sock->write(pack->data,pack->packet_size()+pack->packet_prefix_size(),server_data_port);
//  fprintf(stderr,"2");
//  { time_marker now,start; while (now.diff_time(&start)<3.0) now.get_time(); }

/*
  unsigned char pk[2]={ CLCMD_REQUEST_RESEND,base->packet.tick_received()};
  if (client_sock->write(pk,2)!=2) return 0;
  fprintf(stderr,"sending retry request to server (tick %d, wait input=%d)\n",pk[1],wait_local_input); */
  return 1;
}

void game_client::add_engine_input()
{
  net_packet *pack=&base->packet;
  base->input_state=INPUT_COLLECTING;
  wait_local_input=0;
  pack->set_tick_received(base->current_tick);
  pack->calc_checksum();
  game_sock->write(pack->data,pack->packet_size()+pack->packet_prefix_size(),server_data_port);
//  data_sock->write(pack->data,pack->packet_size()+pack->packet_prefix_size());
/*  fprintf(stderr,"(sending %d)\n",base->packet.tick_received());

  { time_marker now,start; while (now.diff_time(&start)<5.0) now.get_time(); }  */
}


int game_client::end_reload(int disconnect)  // notify evryone you've reloaded the level (at server request)
{
  uint8_t cmd=CLCMD_RELOAD_END;
  if (client_sock->write(&cmd,1)!=1) return 0;
  return 1;
}

int game_client::start_reload()
{
  uint8_t cmd=CLCMD_RELOAD_START;
  if (client_sock->write(&cmd,1)!=1) return 0;
  if (client_sock->read(&cmd,1)!=1) return 0;
  return 1;
}

int kill_net();

int game_client::kill_slackers()
{
  if (base->input_state==INPUT_COLLECTING)
    base->input_state=INPUT_PROCESSING;
  kill_net();
  return 0;        // tell driver to delete us and replace with local client
}

int game_client::quit()
{
  uint8_t cmd=CLCMD_UNJOIN;
  if (client_sock->write(&cmd,1)!=1) return 0;
  if (client_sock->read(&cmd,1)!=1) return 0;
  return 1;
}





game_client::~game_client()
{
  delete client_sock;
  delete server_data_port;
}

