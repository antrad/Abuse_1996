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

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

#include "common.h"

#include "game.h"

#include "linked.h"
#include "console.h"

void console::put_string(char const *st)
{
  while (*st)
  {
    put_char(*st);
    st++;
  }
}

void console::redraw()
{
  if (con_win)
  {
    con_win->clear();
    char *s=screen;
    int dx,dy,xa=fnt->width(),ya=fnt->height(),i,j;
    for (j=0,dy=wy(); j<h; j++,dy+=ya)
    {
      for (i=0,dx=wx(); i<w; i++,s++,dx+=xa)
      {
    if (*s)
      fnt->put_char(con_win->screen,dx,dy,*s);
      }
    }
    fnt->put_char(con_win->screen,wx()+cx*xa,wy()+cy*ya,'_');
  }
}

void console::show()
{
  if (!con_win)
  {
    con_win=wm->new_window(lastx,lasty,screen_w(),screen_h(),NULL,name);
    redraw();
    con_win->screen->SetClip(con_win->x1(),con_win->y1(),con_win->x2()+1,con_win->y2()+1);
  }
}

void console::hide()
{
  if (con_win)
  {
    lastx=con_win->x;
    lasty=con_win->y;
    wm->close_window(con_win);
    con_win=NULL;
  }
}

console::~console()
{
  hide();
  free(screen);
  free(name);
}

console::console(JCFont *font, int width, int height, char const *Name)
{
  con_win=NULL;
  w=width;
  h=height;
  screen=(char *)malloc(w*h);
  memset(screen,' ',w*h);
  cx=cy=0;
  fnt=font;
  lastx=xres/2-screen_w()/2;
  lasty=yres/2-screen_h()/2;
  name = strdup(Name);
}


void console::draw_cursor()
{
  if (con_win)
    fnt->put_char(con_win->screen,cx*fnt->width()+wx(),cy*fnt->height()+wy(),'_');
}


void console::draw_char(int x, int y, char ch)
{
  if (con_win)
  {
    int fw=fnt->width(),fh=fnt->height();
    int dx=wx()+x*fw,dy=wy()+y*fh;
    con_win->screen->bar(dx,dy,dx+fw-1,dy+fh-1,wm->black());
    fnt->put_char(con_win->screen,dx,dy,ch);
  }
}

void console::do_cr()
{
  if (cx<w && cy<h)  draw_char(cx,cy,screen[cy*w+cx]);
  cx=0;
  cy++;
  if (cy>=h)
  {
    cy=h-1;
    if (con_win)
    {
      memmove(screen,screen+w,w*(h-1));
      memset(screen+w*(h-1),' ',w);
      redraw();
      wm->flush_screen();
    }
  } else draw_cursor();
}

void console::put_char(char ch)
{


  switch (ch)
  {
    case JK_BACKSPACE :
    {
      if (cx)
      {
    if (con_win)
      draw_char(cx,cy,screen[cy*w+cx]);
    cx--;
    if (con_win)
      draw_cursor();
      }
    } break;
    case '\n' :
    case JK_ENTER :
    {
      do_cr();
    } break;
    default :
    {
      screen[cy*w+cx]=ch;
      if (con_win)
        draw_char(cx,cy,ch);
      cx++;
      if (cx>=w) do_cr(); else
      if (con_win) draw_cursor();
    }
  }
}


void console::print_f( const char *format, ...)
{
  char st[300];
  va_list ap;
  va_start(ap, format);
  vsprintf(st,format,ap);
  va_end(ap);
  put_string(st);
}


shell_term::shell_term(JCFont *font, int width, int height, char const *Name) :
  console(font,width,height,Name)
{
  shcmd[0]=0;
  prompt();
}

void shell_term::prompt()
{
  put_string("(?=help)>");
}

void shell_term::execute(char const *st)
{
  put_string(st);
  put_string(" : unhandled\n");
}

int shell_term::handle_event(event &ev)
{
  if (ev.window==con_win && con_win)
  {
    switch (ev.type)
    {
      case EV_KEY :
      {
    switch (ev.key)
    {
      case JK_BACKSPACE:
      {
        if (shcmd[0]!=0)
        {
          shcmd[strlen(shcmd)-1]=0;
          put_char(ev.key);
        }
      } break;
      case JK_ENTER :
      {
        put_char(ev.key);
        execute(shcmd);
        prompt();
        shcmd[0]=0;
      } break;
      default :
      {
        if (ev.key<256 && isprint(ev.key))
        {
          int x=strlen(shcmd);
          shcmd[x+1]=0;
          shcmd[x]=ev.key;
          put_char(ev.key);
        }
      } break;
    } break;
      }
    }
    return 1;
  }
  return 0;
}

