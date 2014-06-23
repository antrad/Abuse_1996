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

#include "server2.h"
#include "nfserver.h"
#include "nfclient.h"
#include "dprint.h"
#include "view.h"
#include "jrand.h"
#include "objects.h"
#include "level.h"
#include "dev.h"

int start_running=0;

class client_descriptor
{
  public :
  client_descriptor(int client_number);
  view *player;                // has this player been inducted yet?
  client_descriptor *next;
  long requested_join,cx1,cy1,cx2,cy2;
  int cnum;
} ;

client_descriptor::client_descriptor(int client_number)
{
  player=NULL;
  next=NULL;
  requested_join=0;
  cnum=client_number;
}

game_server *local_server=NULL;       // created on server machine, NULL on all others


game_server::game_server(int argc, char **argv, int port)
{
  client_list=NULL;
  sync_check=0;                  // should we send sync packets to client?

  for (int i=1; i<argc; i++)
  {
    if (!strcmp(argv[i],"-sync"))
    sync_check=1;
  }
}







game_server::~game_server()
{
  for (client_descriptor *p=client_list; p; )
  {
    client_descriptor *q=p;
    p=p->next;
    delete q;
  }
}



void game_server::receive_inputs()         // reads inputs from all non-local clients
{
/*  client_descriptor *last=NULL;
  packet pk;
  for (client_descriptor *p=client_list; p; )
  {
    int delete_me=0;
    if (p->connection)
    {
      if (p->player)                        // clients with players are required to send input
      {
    int error=!get_pkt(p->connection,pk);
    if (!error)
          pk.insert_into(next_out);
    else                                // on error delete the client
      delete_me=1;
      } else
      {
    if (p->connection->ready_to_read())
    {
      if (!p->connection->get(pk))
        delete_me=1;
      else
      {
        uchar cmd;
        long cx1,cy1,cx2,cy2;
        if (!pk.read((uchar *)&cmd,1) || cmd!=SCMD_JOIN_GAME ||
        pk.read((uchar *)&cx1,4)!=4 ||
        pk.read((uchar *)&cy1,4)!=4 ||
        pk.read((uchar *)&cx2,4)!=4 ||
        pk.read((uchar *)&cy2,4)!=4)
          delete_me=1;
        else
        {
          p->cx1=lltl(cx1);
          p->cy1=lltl(cy1);
          p->cx2=lltl(cx2);
          p->cy2=lltl(cy2);
          p->requested_join=1;      // mark this client as wanting to join
          pk.insert_into(next_out);
        }

      }
    }
      }
    }

    if (delete_me)
    {
      client_descriptor *del_me=p;
      p=p->next;
      if (last)
        last->next=p;
      else client_list=p;
      delete del_me;
    } else { last=p; p=p->next; }


  }*/

}

void game_server::send_inputs()            // pass collected inputs to all non-local clients
{
/*  client_descriptor *last=NULL;
  if (sync_check)
  {
    next_out.write_uint8(SCMD_SYNC);
    next_out.write_uint32(make_sync_uint32());
  }

  next_out.write_uint8(SCMD_END_OF_PACKET);        // so clients knows when to stop reading

  for (client_descriptor *p=client_list; p; )
  {
    if (p->connection && p->player)
    {
      int error=!p->connection->send(next_out);
      if (error)
      {
    client_descriptor *del_me=p;
    p=p->next;
    if (last)
      last->next=p;
    else client_list=p;
    delete del_me;
      } else { last=p; p=p->next; }
    } else
    {
      last=p;
      p=p->next;
    }
  }  */
}


void game_server::check_for_clients()
{

}


void game_server::join_new_players()
{
/*  int wait=0;
  client_descriptor *p=client_list;
  for (; p; p=p->next)
    if (p->requested_join)
    {

      view *f=player_list;
      for (; f && f->next; f=f->next);      // find last player, add one for pn
      int i,st=0;
      for (i=0; i<total_objects; i++)
        if (!strcmp(object_names[i],"START"))
      st=i;

      game_object *o=create(current_start_type,0,0);
      game_object *start=current_level->get_random_start(320,NULL);
      if (start) { o->x=start->x; o->y=start->y; }
      else { o->x=100; o->y=100; }

      f->next=new view(o,NULL,f->player_number+1);
      o->set_controller(f->next);

      if (start)
        current_level->add_object_after(o,start);
      else
        current_level->add_object(o);

      view *v=f->next;

      v->cx1=p->cx1;
      v->cy1=p->cy1;
      v->cx2=p->cx2;
      v->cy2=p->cy2;
      p->player=v;
      v->Drawable=p->cnum;

      wait=1;
      p->requested_join=0;
    }


  if (wait)  // wait for acknowedgement from everone then delete net file
  {
    packet pk;
    current_level->save("netstart.spe",1);
    printf("%d sync for save\n",make_sync_uint32());

    client_descriptor *last=NULL;
    for (p=client_list; p; p=p->next)
    {
      if (p->player)
      {
    pk.write_uint8(SCMD_JOIN_START);
    int error=!p->connection->send(pk);
    if (!error)
    {
      while (!p->connection->ready_to_read())
            service_net_request();
    }

    if (error || !p->connection->get(pk))
    {
      if (!last)
        client_list=client_list->next;
      else last->next=p->next;
      delete p;
    } else
         last=p;
      }
    }
    unlink("netstart.spe");
  }*/
}



/*

    server/client interaction


  Client - get/send commands

  Server - receive inputs
           check for join request
       if join request add SCMD_JOIN_GAME to out packet
       send inputs

  Client - read commands from server
           process commands
       tick_game
       draw


  Server (if join request) :
          create new_player
          save level to netstart.spe
      wait for all clients with views to send SCMD_NEW_ACK
      new player should read entire level, while old
      clients seek to "player_info" and read this.






*/
