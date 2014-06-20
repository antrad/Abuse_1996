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

#include <string.h>

#include "common.h"

#include "pmenu.h"

void pmenu::move(int new_x, int new_y)
{
  wm->move_window(bar,new_x,new_y);
}

pmenu::pmenu(int X, int Y, pmenu_item *first, image *screen)
{
  top=first;
  active=NULL;

  int cx1, cy1, cx2, cy2;
  screen->GetClip(cx1, cy1, cx2, cy2);
  if (cx1<X) cx1=X;
  int w = cx2 - cx1 - Jwindow::left_border() - Jwindow::right_border();
  int h = Jwindow::top_border() + Jwindow::bottom_border();

  bar=wm->new_window(X, Y, w, 0, NULL);
  bar->freeze();  // can't drag this window
  bar->screen->widget_bar(0,0,w-1,h-1,wm->bright_color(),wm->medium_color(),
            wm->dark_color());



  int total=0,tx,tw;
  pmenu_item *p=top;
  for (; p; p=p->next) total++;

  tw=w/(total+1);
  tx=tw/2;

  for (p=top; p; p=p->next,tx+=tw)
    p->draw_self(bar,itemx(p),1,itemw(p),1,p==active);
/*  }
  else
  {
    for (p=top; p; p=p->next,tx+=tw)
      p->draw(bar,itemx(p),1,itemw(p),1,p==active);
  }*/

}

pmenu_item::pmenu_item(int ID, char const *Name, char const *on_off_flag, int Hotkey, pmenu_item *Next)
{
  xp=-1;
  id=ID;
  hotkey=Hotkey;
  on_off=on_off_flag;
  if (Name)
    n = strdup(Name);
  else n=NULL;
  next=Next;
  sub=NULL;
}

pmenu_item::pmenu_item(char const *Name, psub_menu *Sub, pmenu_item *Next, int xpos)
{
  xp=xpos;
  id=0; hotkey=-1;
  next=Next;
  on_off=NULL;
  CONDITION(Name,"Sub menu cannot have a NULL name");
  n = strdup(Name);
  sub=Sub;
}

pmenu_item *pmenu_item::find_id(int search_id)
{
  if (id==search_id) return this;
  else if (sub) return sub->find_id(search_id);
  else return NULL;
}

pmenu_item *pmenu_item::find_key(int key)
{
  if (key==hotkey && hotkey!=-1) return this;
  else if (sub) return sub->find_key(key);
  else return NULL;
}

pmenu::~pmenu()
{
  while (top)
  {
    pmenu_item *p=top;
    top=top->next;
    delete p;
  }
  if (bar) wm->close_window(bar);
}

psub_menu::~psub_menu()
{
  if (win)
    wm->close_window(win);

  while (first)
  {
    pmenu_item *tmp=first;
    first=first->next;
    delete tmp;
  }
}

pmenu_item *psub_menu::find_id(int search_id)
{
  for (pmenu_item *f=first; f; f=f->next)
  {
    pmenu_item *ret=f->find_id(search_id);
    if (ret) return ret;
  }
  return NULL;
}

pmenu_item *psub_menu::find_key(int key)
{
  for (pmenu_item *f=first; f; f=f->next)
  {
    pmenu_item *ret=f->find_key(key);
    if (ret) return ret;
  }
  return NULL;
}


void psub_menu::hide(Jwindow *parent, int x, int y)
{
  int w,h;
  calc_size(w,h);
  int cx1, cy1, cx2, cy2;
  screen->GetClip(cx1, cy1, cx2, cy2);
  // FIXME: is this correct? it looks like it used to be incorrect
  // before the GetClip refactoring...
  if (w+x>cx2-1)
    x=cx2-1-w;

  if (win)
  {
    if (active!=-1)
    {
      int w,h;
      calc_size(w,h);
      item_num(active)->draw(win,x+3,y+3+active*(wm->font()->height()+1),w-6,0,0);
    }
    wm->close_window(win);
    win=NULL;
  }
}

void psub_menu::calc_size(int &w, int &h)
{
  int tw=wm->font()->width(),th=wm->font()->height();
  w=h=0;
  for (pmenu_item *p=first; p; p=p->next)
  {
    if (p->name())
    {
      int l=strlen(p->name())*tw+8;
      if (p->on_off) l+=tw*4;
      if (l>w) w=l;
    }
    h++;
  }
  h=h*(th+1)+8;
}

void psub_menu::draw(Jwindow *parent, int x, int y)
{
  if (win) wm->close_window(win);

  int w,h,i=0;
  calc_size(w,h);
  int cx1, cy1, cx2, cy2;
  screen->GetClip(cx1, cy1, cx2, cy2);
  if (parent->x+w+x>=cx2)
    x=cx2-1-w-parent->x;
  if (h+y+parent->y>=cy2)
  {
    if (parent->y+parent->h+wm->font()->height()>=cy2)
      y=-h;
    else y=y-h+wm->font()->height()+5;
  }


  win=wm->new_window(parent->x+x,parent->y+y,
             w - Jwindow::left_border() - Jwindow::right_border(),
             h - Jwindow::top_border() - Jwindow::bottom_border(),
                     NULL);
  win->freeze();
  win->screen->widget_bar(0,0,w-1,h-1,wm->bright_color(),wm->medium_color(),wm->dark_color());

  int has_flags=0;
  pmenu_item *p=first;
  for (; p; p=p->next) if (p->on_off) has_flags=1;
  x=has_flags ? 3+wm->font()->width() : 3;
  y=3;

  for (p=first; p; p=p->next,i++,y+=wm->font()->height()+1)
    p->draw(win,x,y,w-6,0,i==active);

}

void pmenu_item::draw_self(Jwindow *parent, int x, int y, int w, int top, int active)
{
  int bx=x;
  if (on_off) bx=x-wm->font()->width();

  if (!n)
  {
    int h=wm->font()->height();
    parent->screen->widget_bar(x,y+h/2-1,x+w-1,y+h/2,wm->dark_color(),wm->medium_color(),wm->bright_color());
  } else
  {
    if (active)
    {
      if (xp!=-1)
        parent->screen->xor_bar(bx,y,x+w-1,y+wm->font()->height()+1,wm->dark_color());
      else
      {
    parent->screen->bar(bx,y,x+w-1,y+wm->font()->height()+1,wm->dark_color());
    wm->font()->put_string(parent->screen,x+1,y+1,n,wm->medium_color());
    if (on_off && *on_off) wm->font()->put_string(parent->screen,bx+1,y+1,"*",wm->medium_color());
      }
    } else
    {
      if (xp!=-1)
        parent->screen->xor_bar(bx,y,x+w-1,y+wm->font()->height()+1,wm->dark_color());
      else
      {
    parent->screen->bar(bx,y,x+w-1,y+wm->font()->height()+1,wm->medium_color());
    wm->font()->put_string(parent->screen,x+1,y+1,n,wm->bright_color());
    if (on_off && *on_off) wm->font()->put_string(parent->screen,bx+1,y+1,"*",wm->bright_color());
      }
    }
  }
}

void pmenu_item::draw(Jwindow *parent, int x, int y, int w, int top,
              int active)
{
  if (n)
  {
    if (active)
    {
      draw_self(parent,x,y,w,top,active);
      if (sub)
      {
    if (top)
          sub->draw(parent,x,y+wm->font()->height()+2);
    else
      sub->draw(parent,x+w,y);
      }
    }
    else
    {
      if (sub)
      {
    if (top)
          sub->hide(parent,x,y+wm->font()->height()+2);
    else
      sub->hide(parent,x+w,y);
      }
      draw_self(parent,x,y,w,top,active);

    }

  } else draw_self(parent,x,y,w,top,active);
}

int pmenu::itemx(pmenu_item *p)
{
  if (p->xp!=-1) return p->xp;
  int w=bar->screen->Size().x;


  int total=0,tw,i=0,x=0;
  for (pmenu_item *pp=top; pp; pp=pp->next,i++)
  { if (pp==p) x=i;
    total++;
  }


  tw=w/(total+1);
  return tw/2+x*tw;
}


void pmenu::draw(image *screen, int top_only)
{

}


int psub_menu::handle_event(Jwindow *parent, int x, int y, event &ev)
{
  int w,h;
  calc_size(w,h);
  int cx1, cy1, cx2, cy2;
  screen->GetClip(cx1, cy1, cx2, cy2);

  x=win->x;
  y=win->y;

  int has_flags=0,dx=3;
  for (pmenu_item *p=first; p; p=p->next) if (p->on_off) has_flags=1;
  if (has_flags) dx+=wm->font()->width();

  int th=wm->font()->height();
  if (ev.mouse_move.x>=x && ev.mouse_move.y>=y && ev.mouse_move.x<x+w && ev.mouse_move.y<y+h)
  {
    int new_active=(ev.mouse_move.y-y-3)/(th+1);
    if (item_num(new_active)==NULL) new_active=-1;

    if (new_active!=active)
    {
      if (active!=-1)
        item_num(active)->draw(win,dx,3+active*(th+1),w-6,0,0);
      active=new_active;
      if (active!=-1)
        item_num(active)->draw(win,dx,3+active*(th+1),w-6,0,1);
    }
    if (ev.type==EV_MOUSE_BUTTON)
    {
      if (active!=-1)
        return item_num(active)->handle_event(win,dx,3+active*(th+1),w-6,0,ev);
      else return 0;
    } else return 1;
  } else if (active!=-1)
    return item_num(active)->handle_event(win,win->x+dx,win->y+3+active*(th+1),w-6,0,ev);
  else return 0;


}

int pmenu_item::handle_event(Jwindow *parent, int x, int y, int w, int top,
                 event &ev)
{
  x+=parent->x;
  y+=parent->y;
  if (ev.mouse_move.x>=x && ev.mouse_move.y>=y && ev.mouse_move.x<x+w &&
      ev.mouse_move.y<y+wm->font()->height()+2)
  {
    if (sub) return 1;
    else
    {
      if (ev.type==EV_MOUSE_BUTTON &&n)
        wm->push_event(new event(id,(char *)this));
      return 1;
    }
  } else if (sub)
  {
    if (top)
      return sub->handle_event(parent,x,y+wm->font()->height()+2,ev);
    else return sub->handle_event(parent,x+w,y,ev);
  } else return 0;
}

pmenu_item *pmenu::inarea(int mx, int my, image *screen)
{
  int cx1, cy1, cx2, cy2;
  screen->GetClip(cx1, cy1, cx2, cy2);
  mx-=bar->x;
  my-=bar->y;
  if (mx<0 || my<0 || mx>=bar->screen->Size().x || my>=bar->screen->Size().y) return NULL;
  else
  {
    for (pmenu_item *p=top; p; p=p->next)
    {
      if (!p->next) return p;
      else if (itemx(p->next)>mx) return p;
    }
    return NULL;
  }
}

int psub_menu::own_event(event &ev)
{
  if (win && ev.window==win) return 1; else
    for (pmenu_item *p=first; p; p=p->next)
      if (p->own_event(ev))
        return 1;
  return 0;
}

int pmenu_item::own_event(event &ev)
{
  if (sub)
    return sub->own_event(ev);
  else return 0;
}

pmenu_item::~pmenu_item()
{ if (n) free(n); if (sub) delete sub;
}

int pmenu::handle_event(event &ev, image *screen)
{
  if (!active && ev.window!=bar) return 0;
/*
    int yes=0;
    if (ev.window==bar) yes=1;    // event in top bar?
    else
    {
      for (pmenu_item *p=top; p && !yes; p=p->next)  // event in submenu?
      if (p->own_event(ev)) yes=1;
    }
    if (!yes) return 0;        // event is not for us...
  }*/

  switch (ev.type)
  {
    case EV_KEY :
    {
      for (pmenu_item *p=top; p; p=p->next)
      {
    pmenu_item *r=p->find_key(ev.key);
    if (r)
    {
      wm->push_event(new event(r->id,(char *)r));
      return 1;
    }
      }
      return 0;
    } break;
    case EV_MOUSE_MOVE :
    {
      pmenu_item *new_selection=inarea(ev.mouse_move.x,ev.mouse_move.y,screen);
      if (!new_selection && active &&
      active->handle_event(bar,itemx(active),1,itemw(active),1,ev))
    return 1;
      else if (active!=new_selection)
      {
    if (active)
      active->draw(bar,itemx(active),1,itemw(active),1,0);
    active=new_selection;
    if (active)
      active->draw(bar,itemx(active),1,itemw(active),1,1);
      }
      if (active) return 1;
      else return 0;
    } break;
    case EV_MOUSE_BUTTON :
    {
      if (active)
      {
        if (active->handle_event(bar,itemx(active),1,itemw(active),1,ev))
    {
      active->draw(bar,itemx(active),1,itemw(active),1,0);
      active=NULL;
      return 1;
    } else return 0;
      }
      else return 0;
    } break;
  }
  return 0;
}


