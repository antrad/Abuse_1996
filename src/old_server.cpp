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

#include <unistd.h>

#include "nfserver.h"

#include "server.h"
#include "view.h"
#include "dprint.h"
#include "jnet.h"
#include "level.h"
#include "game.h"
#include "jrand.h"
#include "timing.h"
//#include "nfserver.h"
//#include "nfclient.h"

//nfs_server *file_server=NULL;

int send_pkt(out_socket *o, packet &pk)
{
//  while (!o->ready_to_write())
//    if (file_server) file_server->service_request();
  return o->send(pk);
}



int get_pkt(out_socket *o, packet &pk)
{
//  while (!o->ready_to_read())
//    if (file_server) file_server->service_request();
  return o->get(pk);
}




server *game_server;
int start_running=0,sync_check=0;

void server::remove_from_server(view *f)  // should only be called from client side
{
  if (f->connect)
  {
    packet pk;
    uint8_t cmd=SCMD_QUIT;    // send quit command to server
    pk.write(&cmd,1);
    send_pkt(f->connect,pk);
    delete f->connect;
    f->connect=NULL;
  }
}

server::server(int argc, char **argv)
{
  char name[200];
  int port=20202,no_net=0;
  strcpy(name,getlogin() ? getlogin() : "unknown");
  in=NULL;

  // see if this computer is net capable


  int i;
  // preprocessing stuff before checking for connect to server
  for (i=1; i<argc; i++)
  {
    if (!strcmp(argv[i],"-port"))
    {
      i++;
      if (sscanf(argv[i],"%d",&port)!=1 || port<1 || port>0xffff)
      {
    dprintf("Bad port number, port should be 1..%d\n",0xffff);
    exit(0);
      }
    } else if (!strcmp(argv[i],"-name"))     // name player uses when connecting
    {
      i++;
      strcpy(name,argv[i]);
    }  else if (!strcmp(argv[i],"-nonet"))
    {
      dprintf("Network bypassed, no player will be able to connect\n");
      no_net=1;
    } else if (!strcmp(argv[i],"-sync"))
      sync_check=1;

  }

  if (no_net)
    has_net=0;
  else has_net=net_init();

  for (i=1; i<argc; i++)
  {
    if (!strcmp(argv[i],"-net"))
    {
      if (!has_net)
      {
    dprintf("No network detected, load network drivers and try again\n");
    exit(1);
      }
      else
      {
    out_socket *os=NULL;
    i++;
    dprintf("Trying to connect to server %s on port %d\n",argv[i],port);
    if (!os=create_out_socket(argv[i],port))
    {
      dprintf("%s\n",last_sock_err);
      dprintf("Make sure server is running...\n");
      exit(1);
    }
    dprintf("Connected!\n");

    join_game(os,name,argv[i]);

      }
    }
  }

  if (!player_list)                  // if we are not connecting to a server, become one
  {
    is_server=1;
    if (has_net)
    {
      in=new in_socket(port);
      if (current_sock_err)
      {
    dprintf("%s\n",last_sock_err);
    dprintf("Running single player mode\n");
    has_net=0;
      } //else
//    file_server=new nfs_server(port+1);

    }
    set_local_players(1);
  }

}





void server::tick()
{
//  if (file_server)
//    file_server->service_request();
  next_out.reset();          // clear the next packet out..
  check_for_new_players();
  collect_inputs();
}

uint32_t make_sync_uint32()
{
  uint32_t x=0;
  for (view *v=player_list; v; v=v->next)
  {
    x^=v->focus->x;
    x^=v->focus->y;
  }
  return x^rand_on;
}

int server::process_command(view *f, uint8_t command, packet &pk)
{
  switch (command)
  {
    case SCMD_QUIT :                          // delete player
    {
      dprintf("Player %d has quit\n",f->player_number);
      return 0;
    } break;

    case SCMD_VIEW_RESIZE :                          // change view area
    {
      uint32_t view_size[8];
      if (pk.read((uint8_t *)view_size,8*4)!=8*4)
      return 0;
      else
      {
    f->resize_view(lltl(view_size[0]),lltl(view_size[1]),lltl(view_size[2]),lltl(view_size[3]));
    f->pan_x=lltl(view_size[4]);
    f->pan_y=lltl(view_size[5]);
    f->shift_down=lltl(view_size[6]);
    f->shift_right=lltl(view_size[7]);
    f->suggest.send_view=0;
    if (is_server)                  // if we are a server, tell everybody about this.
    {
      uint8_t cmd=SCMD_VIEW_RESIZE;
      next_out.write((uint8_t *)&cmd,1);
      uint16_t pn=lstl(f->player_number);
      next_out.write((uint8_t *)&pn,2);
      next_out.write((uint8_t *)view_size,8*4);
    }
      }
    } break;

    case SCMD_WEAPON_CHANGE :                          // change weapon
    {
      uint32_t new_weap;
      if (pk.read((uint8_t *)&new_weap,4)!=4)
        return 0;
      else
      {
    f->current_weapon=lltl(new_weap);
    f->suggest.send_weapon_change=0;
    if (is_server)                      // if we are a server, tell everybody about this.
    {
      uint8_t cmd=SCMD_WEAPON_CHANGE;
      next_out.write((uint8_t *)&cmd,1);
      uint16_t pn=lstl(f->player_number);
      next_out.write((uint8_t *)&pn,2);
      next_out.write((uint8_t *)&new_weap,4);
    }
      }
    } break;


    case SCMD_SET_INPUT :                        // set the input from this player
    {
      signed char inp[5];
      if (pk.read((uint8_t *)inp,5)!=5)
        return 0;
      else
        f->set_input(inp[0],inp[1],inp[2],inp[3],inp[4]);
    } break;

    case SCMD_ADD_VIEW :
    {
      view *v=add_view(pk);
      if (v)
      {
    for (view *f=player_list; f && f->next; f=f->next);
    if (f) f->next=v;
    else player_list=f;
      }
    } break;
    case SCMD_SYNC :
    {
      uint32_t x;
      if (pk.read((uint8_t *)&x,4)!=4)
        return 0;
      else
      {
    uint32_t s=make_sync_uint32();
    if (lltl(x)!=s)
      printf("Out of sync, %x!=%x\n",lltl(x),s);
    return 1;
      }
    } break;

    default :
      return 0;
  }
  return 1;
}

void server::add_change_log(view *f, packet &pk, int number)
{
  if (f->view_changed())
  {
    uint8_t cmd=SCMD_VIEW_RESIZE;
    pk.write(&cmd,1);
    if (number)
    {
      uint16_t pn=lstl(f->player_number);
      pk.write((uint8_t *)&pn,2);
      dprintf("Server : %s resized view %d %d %d %d\n",f->name,
          f->suggest.cx1,f->suggest.cy1,f->suggest.cx2,f->suggest.cy2);
      f->resize_view(f->suggest.cx1,f->suggest.cy1,f->suggest.cx2,f->suggest.cy2);
      f->suggest.send_view=0;
    } else dprintf("sending resize to server\n");
    uint32_t view_size[8];
    view_size[0]=lltl(f->suggest.cx1);
    view_size[1]=lltl(f->suggest.cy1);
    view_size[2]=lltl(f->suggest.cx2);
    view_size[3]=lltl(f->suggest.cy2);
    view_size[4]=lltl(f->suggest.pan_x);
    view_size[5]=lltl(f->suggest.pan_y);
    view_size[6]=lltl(f->suggest.shift_down);
    view_size[7]=lltl(f->suggest.shift_right);
    pk.write((uint8_t *)view_size,8*4);
  }

  if (f->weapon_changed())
  {
    uint8_t cmd=SCMD_WEAPON_CHANGE;
    pk.write(&cmd,1);
    if (number)
    {
      uint16_t pn=lstl(f->player_number);
      pk.write((uint8_t *)&pn,2);
      dprintf("Server : %s change weapon to %d\n",f->name,f->suggest.new_weapon);
      f->current_weapon=f->suggest.new_weapon;
      f->suggest.send_weapon_change=0;
    } else dprintf("sending resize to server\n");
    uint32_t nw=lltl(f->suggest.new_weapon);
    pk.write((uint8_t *)&nw,4);
  }
}

int server::send_inputs(view *f)
{
  packet pk;
  add_change_log(f,pk,0);
  signed char inp[6];
  inp[0]=SCMD_SET_INPUT;
  inp[1]=f->x_suggestion;
  inp[2]=f->y_suggestion;
  inp[3]=f->b1_suggestion;
  inp[4]=f->b2_suggestion;
  inp[5]=f->b3_suggestion;
  if (pk.write((uint8_t *)inp,6)!=6)
    return 0;
  if (!send_pkt(f->connect,pk))
    return 0;
  return 1;
}


void server::collect_inputs()
{
  out_socket *collect_server=NULL;
  for (view *f=player_list; f; )
  {
    view *next=f->next;
    if (is_server)
    {
      if (f->connect)
      {
    packet pk;
    if (get_pkt(f->connect,pk))
    {
      while (!pk.eop())
      {
        uint8_t cmd;
        if (pk.read((uint8_t *)&cmd,1)==1)
          if (!process_command(f,cmd,pk))
          { remove_player(f); f=NULL; }
      }
    } else
    {
      remove_player(f);
      f=NULL;
    }

      } else
      {
        f->get_input();
    add_change_log(f,next_out,1);
      }
    }
    else
    {
      if (f->local_player())
      {
        f->get_input();
    if (f->connect && !send_inputs(f))
          remove_from_server(f);
    else if (f->connect)
          collect_server=f->connect;  // take note that we should collect the input back from the server
      }
    }
    f=next;
  }

  if (collect_server)
  {
    packet pk;
    if (!get_pkt(collect_server,pk))
    {
      for (view *f=player_list; f; f=f->next)
        if (f->local_player())
      remove_from_server(f);
    }

    if (!client_do_packet(pk))
      printf("Error occurred while processing packet from server\n");
  }

  if (is_server && in)
    distribute_changes();

}


void server::distribute_changes()
{
  char cmd;

  for (view *f=player_list; f; f=f->next)
  {
    cmd=SCMD_SET_INPUT;
    next_out.write((uint8_t *)&cmd,1);
    uint16_t pn=lstl(f->player_number);
    next_out.write((uint8_t *)&pn,2);

    signed char inp[5];
    inp[0]=f->x_suggestion;
    inp[1]=f->y_suggestion;
    inp[2]=f->b1_suggestion;
    inp[3]=f->b2_suggestion;
    inp[4]=f->b3_suggestion;
    next_out.write((uint8_t *)inp,5);
  }

  if (sync_check)
  {
    cmd=SCMD_SYNC;
    uint32_t x=lltl(make_sync_uint32());
    next_out.write((uint8_t *)&cmd,1);
    next_out.write((uint8_t *)&x,4);
  }

  for (f=player_list; f; )
  {
    view *n=f->next;
    if (!f->local_player() && f->connect)
      if (!send_pkt(f->connect,next_out))
        remove_player(f);
    f=n;
  }

}

void server::check_for_new_players()
{
  if (is_server && has_net)
  {
    out_socket *nd=in->check_for_connect();
    if (nd)
    {
      packet pk;
//      pk.write_uint32(file_server->get_port());
      if (!send_pkt(nd,pk))
      {
    printf("error writing to connection\n");
    return ;
      }

//      while (!file_server->service_request()) milli_wait(1000);

      if (!get_pkt(nd,pk))
      {
    printf("error reading from connection\n");
    return ;
      } else
      {

    char name[100];
    pk.get_string(name,100);
    printf("Joined by player %s\n",name);
    pk.reset();
    uint8_t ok=1;
    pk.write((uint8_t *)&ok,1);      // write ok to join
    send_pkt(nd,pk);

    /**************** Read suggested view size from client ****/
    if (!get_pkt(nd,pk))
    {
      printf("error reading view info from connection\n");
      return ;
    }
    int32_t cx1,cy1,cx2,cy2;
    if (pk.read((uint8_t *)&cx1,4)!=4) return ;  cx1=lltl(cx1);
    if (pk.read((uint8_t *)&cy1,4)!=4) return ;  cy1=lltl(cy1);
    if (pk.read((uint8_t *)&cx2,4)!=4) return ;  cx2=lltl(cx2);
    if (pk.read((uint8_t *)&cy2,4)!=4) return ;  cy2=lltl(cy2);

    /**************** Create the player  *******************/
    for (view *f=player_list; f && f->next; f=f->next);      // find last player, add one for pn
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

    current_level->add_object(o);
    view *v=f->next;
    v->cx1=cx1;
    v->cy1=cy1;
    v->cx2=cx2;
    v->cy2=cy2;
    v->connect=nd;
    strcpy(v->name,name);


    if (current_level->send(nd))
    {
      uint8_t cmd=SCMD_ADD_VIEW;
      next_out.write((uint8_t *)&cmd,1);
      v->write_packet(next_out);


      /********** Send all of the views to the player **********/
      pk.reset();
      uint16_t tv=0;
      for (f=player_list; f; f=f->next) tv++;
      tv=lstl(tv);
      pk.write((uint8_t *)&tv,2);
      if (!send_pkt(nd,pk)) return ;

      for (f=player_list; f; f=f->next)
      {
        pk.reset();
        f->write_packet(pk);
        if (!send_pkt(nd,pk)) return ;
      }

      pk.reset();
      uint16_t r=lstl(rand_on);
      pk.write((uint8_t *)&r,2);       // write current random seed
      pk.write((uint8_t *)rtable,1024*2);
      send_pkt(nd,pk);

    }
      }
    }
  }
}




int server::join_game(out_socket *os, char *name, char *server_name)
{
  char *re="Error occurred while reading from server\n";
  packet pk;

  if (!get_pkt(os,pk))                  // read join status packet, 0 means we can't join
  { fputs(re,stderr); exit(0); }
  int32_t nfs_port;
  if (pk.read((uint8_t *)&nfs_port,4)!=4)
  { fputs(re,stderr); exit(0); }

//  connect_to_nfs_server(server_name,lltl(nfs_port));




  pk.write((uint8_t *)name,strlen(name)+1);  // send or name and see if it's ok to join in
  if (!send_pkt(os,pk))
  {
    printf("Unable to write to server\n");
    exit(0);
  }

  if (!get_pkt(os,pk))                  // read join status packet, 0 means we can't join
  { fputs(re,stderr); exit(0); }

  uint8_t stat;
  if (pk.read((uint8_t *)&stat,1)!=1)
  { fputs(re,stderr); exit(0); }

  if (stat==0)
  {
    printf("Sorry, this server is refusing you (%s)\n",name);
    exit(0);
  }


  if (current_level)
    delete current_level;

  int32_t vs[4]={ lltl(320/2-155),lltl(200/2-95),lltl(320/2+155),lltl(200/2+70)};
  pk.write((uint8_t *)vs,4*4);
  if (!send_pkt(os,pk))   { printf("Unable to write to server\n"); exit(0);  }


  current_level=new level(os);
  if (current_level->load_failed())
  {
    printf("Error occurred while downloading level\n");
    exit(1);
  }

  if (!get_pkt(os,pk))
  {
    printf("Unable to read views from server\n");
    exit(0);
  }
  uint16_t tv;
  if (pk.read((uint8_t *)&tv,2)!=2)
  { fputs(re,stderr); exit(0); }
  tv=lstl(tv);
  view *last=NULL;
  for (int i=0; i<tv; i++)
  {
    if (!get_pkt(os,pk)) { fputs(re,stderr); exit(0); }

    view *v=add_view(pk);
    if (v)
    {
      printf("added view %d\n",v->player_number);
      if (last)
        last->next=v;
      else player_list=v;
      last=v;
    } else printf("no view created, why?\n");

  }

  if (!get_pkt(os,pk)) { fputs(re,stderr); exit(0); }
  if (pk.read((uint8_t *)&rand_on,2)!=2)    // read the current random seed used by the server.
  { fputs(re,stderr); exit(0); }
  rand_on=lstl(rand_on);
  uint16_t rtab[1024];
  if (!pk.read((uint8_t *)rtab,1024*2)) { fputs(re,stderr); exit(0); }  // read the rand table

  for (int j=0; j<1024*2; j++)
    if (((uint8_t *)rtab)[j]!=((uint8_t *)rtable)[j])
    { printf("rtables differ on byte %d\n",j); exit(0); }

  if (last)
  {
    last->Drawable=1;
    last->connect=os;
  }

  start_running=1;
  is_server=0;
  return 1;
}






void server::remove_player(view *f)
{
  uint8_t cmd=SCMD_REMOVE_VIEW;
  next_out.write((uint8_t *)&cmd,1);
  uint16_t pn=lstl(f->player_number);
  next_out.write((uint8_t *)&pn,2);
  if (f==player_list)
    player_list=player_list->next;
  else
  {
    for (view *v=player_list; v && v->next!=f; v=v->next);
    v->next=f->next;
  }

  if (f->connect) delete f->connect;
  f->focus->set_controller(NULL);
  delete f;
}


/*int server::send_level(net_descriptor *os)
{

      cmd=SCMD_ADD_VIEW;
      next_out.write((uint8_t *)&cmd,1);

    uint16_t pn=lstl(new_player->player_number);
    next_out.write((uint8_t *)&pn,2);
    uint16_t type=lstlli(new_player->focus->otype);
    next_out.write((uint8_t *)&type,2);
    uint32_t x=lltl(new_player->focus->x),y=lltl(new_player->focus->y);
    next_out.write((uint8_t *)&x,4);
    next_out.write((uint8_t *)&y,4);
  }*/



#define TOT_VIEW_VARS 32
view *server::add_view(packet &pk)
{
  uint32_t x[TOT_VIEW_VARS];
  if (!pk.read((uint8_t *)x,TOT_VIEW_VARS*4)) return NULL;
  for (int i=0; i<TOT_VIEW_VARS; i++) x[i]=lltl(x[i]);
  int skip=0;
  for (view *f=player_list; f; f=f->next)
    if (f->player_number==x[0])
      skip=1;

  if (skip)
  {
    pk.advance(total_objects*4);
    char nm[200];
    pk.get_string(nm,100);
    return NULL;
  }
  else
  {
    game_object *o=current_level->number_to_object(x[24]);
    if (!o)
    {
      o=create(x[25],x[26],x[27]);
      current_level->add_object(o);
    }
    view *v=new view(o,NULL,x[0]);
    o->set_controller(v);

    v->cx1=x[1];   v->cy1=x[2];   v->cx2=x[3];  v->cy2=x[4];
    v->lives=x[5];
    v->pan_x=x[6];       v->pan_y=x[7];
    v->no_xleft=x[8];    v->no_xright=x[9];  v->no_ytop=x[10];  v->no_ybottom=x[11];
    v->last_x=x[12];     v->last_y=x[13];
    v->last_left=x[14];  v->last_right=x[15]; v->last_up=x[16]; v->last_down=x[17];
    v->last_b1=x[18];    v->last_b2=x[19];    v->last_b3=x[20]; v->last_hp=x[21];
    v->last_ammo=x[22];  v->last_type=x[23]; v->visor_time=x[28]; v->current_weapon=x[29];
    v->secrets=x[30];    v->kills=x[31];

    pk.read((uint8_t *)v->weapons,total_objects*4);
    pk.get_string(v->name,100);


    return v;
  }
}






int server::client_do_packet(packet &pk)
{
  int rp=pk.get_read_position();

  int er=0;
  while (!pk.eop() && !er)
  {
    uint8_t cmd;
    if (pk.read(&cmd,1)!=1)
      er=1;
    else
    {
      view *f=NULL;
      int fail=0;
      if (cmd!=SCMD_ADD_VIEW && cmd!=SCMD_SYNC)
      {
    uint16_t player;
    if (pk.read((uint8_t *)&player,2)!=2)
      er=1;
    player=lstl(player);
    for (f=player_list; f && f->player_number!=player; f=f->next);
    if (!f) fail=1;
      }
      if (!fail)
      {
    if (!process_command(f,cmd,pk))
      er=1;
      }
      else
    er=1;
    }
  }
  pk.set_read_position(rp);
  return !er;
}



server::~server()
{
  if (in) delete in;
}


