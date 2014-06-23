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

#include "go.h"
#include "level.h"
#include "game.h"
#include "id.h"
#include "signals.h"

#define EL_WAIT_MOVEMENT 1           // wait for the user to press up or down
#define EL_SKIP_SIGNAL   2
#define EL_WAIT_SIGNAL   3
#define EL_WAIT_SKIP     4


void elcontrol::draw()
{
  if (dev & EDIT_MODE)
    game_object::draw();

}

ifield *elevator::make_fields(int ystart, ifield *Next)
{
  int H=10;
  return new text_field(5,ystart+H*0,ELEVATOR_SPEED,"speed",              "#####",speed,
         new text_field(5,ystart+H*1,ELEVATOR_DIR,"heading dir",          "#####",dir,
         new text_field(5,ystart+H*2,ELEVATOR_DIRECTION,"facing dir",     "#####",direction,
            NULL)));
}

void elevator::gather_input(InputManager *inm)
{
  dir=atoi(inm->get(ELEVATOR_DIR)->read());
  speed=atoi(inm->get(ELEVATOR_SPEED)->read());
  direction=atoi(inm->get(ELEVATOR_DIRECTION)->read());
  if (direction==0) direction=1;
}



ifield *elcontrol::make_fields(int ystart,ifield *Next)
{
  int H=10;
  return new text_field(5,ystart+H*0,ELCONTROL_ALLOW_DIR,"stop dir","#####",allow_dir,NULL);
}

void elcontrol::gather_input(InputManager *inm)
{
  allow_dir=atoi(inm->get(ELCONTROL_ALLOW_DIR)->read());
}


elcontrol::elcontrol(long X, long Y)
{
  defaults();
  x=X;
  y=Y;
  allow_dir=0;
}

elcontrol::elcontrol(FILE *fp, unsigned char *state_remap)
{
  load(fp,state_remap);
  allow_dir=read_uint16(fp);
}





int elevator::can_block(game_object *who)
{
  if (who!=this)
    return 1;
  else return 0;

}


void elevator::draw()  // draw cables above the elevator
{
  game_object::draw();
  long x1,y1,x2,y2,sy1,sy2,sx,i;
  picture_space(x1,y1,x2,y2);

  sx=the_game->screenx(x1);

  sy2=the_game->screeny(y1);
  if (sy2>=the_game->viewy1)
  {
    long draw_to=y1-(sy2-the_game->viewy1),tmp=x;
    current_level->foreground_intersect(x,y1,tmp,draw_to);
    sy1=the_game->screeny(draw_to);
    sy1=max(the_game->viewy1,sy1);
    sy2=min(the_game->viewy2,sy2);
    TransImage *p=picture();

    for (i=sy1; i<=sy2; i++)
      p->PutScanLine(screen,sx,i,0);
  }
}


elevator::elevator(long X, long Y)
{
  defaults();
  x=X;
  y=Y;
  dir=0;
  speed=3;
}

int elevator::size()
{
  return game_object::size()+4;
}


elevator::elevator(FILE *fp, unsigned char *state_remap)
{
  load(fp,state_remap);
  dir=read_uint16(fp);
  speed=read_uint16(fp);
}

void elevator::save(FILE *fp)
{
  game_object::save(fp);
  write_uint16(fp,dir);
  write_uint16(fp,speed);
}


elcontrol *elevator::find_stop()
{
  long x1,y1,x2,y2;
  picture_space(x1,y1,x2,y2);
  int i;
  game_object **o=current_level->obj;
  for (i=current_level->first_object(); i>=0; i=o[i]->next_active)
  {
    if (o[i]->type()==O_elcontrol)
    {
      int yd=o[i]->y-y;
      if (abs(yd)<=speed && o[i]->x>x1 && o[i]->x<x2)
        return (elcontrol *)(o[i]);
    }

  }
  return NULL;
}



int elevator::decide()
{
  if (abs(dir)<=1)                                  // the elevator is stopped
  {
    switch (state)
    {
      case stopped :
      {
        long x1,y1,x2,y2;
    picture_space(x1,y1,x2,y2);


    game_object *a=current_level->attacker(this);
                  // are we in the elevator?, if not wait for someone to get in
    if (a->x>x1+3 && a->x<x2-3 && a->y<y2 && a->y>y1+8)
    {
      int but,xm,ym;                          // wait for the user to press up or down
      the_game->get_movement(but,xm,ym);      // see which way he wants to go
      if (ym)
      {
        elcontrol *sp=find_stop();
        if (ym<0 && (!sp || sp->allow_dir>=0))
          dir=-1;
        else if (ym>0 && (!sp || sp->allow_dir<=0))
          dir=1;
        if (dir)
        {
          if (has_sequence(start_still_jump))
            set_state(start_still_jump);         // close the door
          else
            dir=dir+dir;                     // elevator doesn't have a door, start moving
        }
      }
    }
    break;
      }
      case start_still_jump :
    next_picture();
    if (end_of_sequence())                // door closed yet?
    {
      set_state(still_jump);              // we can start moving now
      dir=dir+dir;
    }
    break;
      case still_jump :                       // just stopped, open the doors
    set_state(end_still_jump);
    break;
      case end_still_jump :
    next_picture();
    if (end_of_sequence())                // wait for doors to finish opening
      set_state(stopped);
    break;
      default :
    CHECK(0);
    }
  }

  if (abs(dir)>1)                     // are we moving?
  {
    next_picture();
    int adder;
    if (dir<0)                         // how much do we move this tick?
      adder=-speed;
    else adder=speed;

    if (abs(dir)==8)                   // are we checking for stops yet?
    {
      elcontrol *s=find_stop();
      if (s)                           // found one, stop or slow down
      {
    adder=s->y-y;
    if (!adder)
          dir=0;
      }
    } else dir=dir+dir;

    if (dir)                                   // make sure move, wasn't canceled by a stop
    {
      if (!current_level->crush(this,0,adder)) // see if we can crush anyone (I hope so! :) )
      {
    y+=adder;
    current_level->push_characters(this,0,adder);
      } else dir=0;                           // we crushed, stop the elevator and wait on user
    }
  }

  return 1;                                   // return not dead, can't kill elevators
}


char *sensor::aname()
{
  if (activate==-1)
    return "FOCUS";
  else return object_names[activate];
}

void sensor::get_activate(char *name)
{
  int i;
  activate=-1;  // if we can't fnd the name then go focus
  for (i=0; i<TOTAL_OBJECTS; i++)
    if (!strcmp(name,object_names[i]))
      activate=i;
}




int sensor::size()
{
  return game_object::size()+2*3+(strlen(aname())+2);
}


void sensor::save(FILE *fp)
{ game_object::save(fp);
  write_uint16(fp,xrange);
  write_uint16(fp,yrange);
  write_uint16(fp,signal);

  write_uint8(fp,strlen(aname())+1);
  fwrite(aname(),strlen(aname())+1,1,fp);
}

sensor::sensor(FILE *fp, unsigned char *state_remap)
{
  load(fp,state_remap);
  xrange=read_uint16(fp);
  yrange=read_uint16(fp);
  signal=read_uint16(fp);

  char name[200];
  fread(name,read_uint8(fp),1,fp);
  get_activate(name);
}

int sensor::decide()
{
  int i;
  game_object **o=current_level->obj;
  long x1,y1,x2,y2;
  if (activate==-1)
  {
    current_level->attacker(this)->picture_space(x1,y1,x2,y2);
    if (x+xrange>=x1 && x-xrange<=x2 && y+yrange>=y1 && y-yrange<=y2)
      current_level->send_signal(signal);
  }
  else
  {
    for (i=current_level->first_object(); i>=0; i=o[i]->next_active)
    {
      long x1,y1,x2,y2;
      if (o[i]->type()==activate)
      {
    o[i]->picture_space(x1,y1,x2,y2);
    if (x+xrange>=x1 && x-xrange<=x2 && y+yrange>=y1 && y-yrange<=y2)
    {
          current_level->send_signal(signal);
      return 1;          // only send one signal!
    }
      }
    }
  }
  return 1;
}




void sensor::draw()
{
  if (dev & EDIT_MODE)
  {
    game_object::draw();
    int sx=the_game->screenx(x),sy=the_game->screeny(y);
    screen->rectangle(sx-xrange,sy-yrange,sx+xrange,sy+yrange,wm->bright_color());
  }
}


ifield *sensor::make_fields(int ystart, ifield *Next)
{
  int H=10;
  return new text_field(5,ystart+H*0,SENSOR_XRANGE,   "xrange",    "#####",xrange,
     new text_field(5,ystart+H*1,SENSOR_YRANGE,   "yrange",    "#####",yrange,
     new text_field(5,ystart+H*2,SENSOR_SIGNAL,   "signal",    "#####",signal,
     new text_field(5,ystart+H*3,SENSOR_ACTIVATE, "activator", "####################",aname(),
            NULL))));
}


void sensor::gather_input(InputManager *inm)
{
  xrange=atoi(inm->get(SENSOR_XRANGE)->read());
  yrange=atoi(inm->get(SENSOR_XRANGE)->read());
  signal=atoi(inm->get(SENSOR_SIGNAL)->read());
  get_activate(inm->get(SENSOR_ACTIVATE)->read());
}

