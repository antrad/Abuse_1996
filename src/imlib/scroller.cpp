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

#include "common.h"

#include "scroller.h"
#define HS_ICON_W 10
#define HS_ICON_H 8


uint8_t hs_left_arrow[10*8]={
    0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
    0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 2, 0, 1, 1, 2, 2, 2, 2, 2, 2, 2,
    0, 0, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
    2, 0, 0, 0, 0};


uint8_t hs_right_arrow[10*8]={
    0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0,
    0, 0, 0, 0, 0, 2, 1, 1, 0, 0, 0, 0, 0, 0, 2,
    1, 1, 0, 0, 0};


uint8_t vs_up_arrow[8*10]={
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 0,
    0, 0, 0, 1, 1, 1, 1, 2, 0, 0, 1, 2, 1, 1, 2,
    1, 2, 0, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0, 1, 1,
    2, 0, 0, 0, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0, 1,
    1, 2, 0, 0, 0, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0,
    0, 2, 2, 0, 0};


uint8_t vs_down_arrow[8*10]={
    0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 1, 1, 2, 0,
    0, 0, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0, 1, 1, 2,
    0, 0, 0, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0, 1, 1,
    2, 0, 0, 0, 1, 2, 1, 1, 2, 1, 2, 0, 0, 1, 1,
    1, 1, 2, 0, 0, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0};

void show_icon(image *screen, int x, int y, int icw, int ich, uint8_t *buf)
{
  int cx1, cy1, cx2, cy2;
  screen->GetClip(cx1, cy1, cx2, cy2);
  uint8_t remap[3];
  remap[0]=wm->medium_color();
  remap[1]=wm->bright_color();
  remap[2]=wm->dark_color();

  screen->Lock();
  for (int yc=ich; yc; yc--,y++)
  {
    if (y >= cy1 && y < cy2)
    {
      uint8_t *sl=screen->scan_line(y)+x;
      for (int xc=icw,xo=x; xc; xc--,xo++,sl++,buf++)
      {
    if (xo >= cx1 && xo < cx2)
      *sl=remap[*buf];
      }
    }
  }
  screen->AddDirty(x, y, x + icw, y + ich);
  screen->Unlock();
}

scroller::scroller(int X, int Y, int ID, int L, int H, int Vert, int Total_items, ifield *Next)
{  x=X; y=Y; id=ID; next=Next;  l=L; h=H;  sx=0;  t=Total_items;  drag=-1; vert=Vert;
}


void scroller::area(int &x1, int &y1, int &x2, int &y2)
{
  area_config();
  x1=x-1; y1=y-1;
  if (vert)
  { x2=x+l+bw();  y2=y+h; }
  else
  { x2=x+l;  y2=y+h+bh(); }
}

void scroller::dragger_area(int &x1, int &y1, int &x2, int &y2)
{
  if (vert)
  { x1=x+l; y1=y+bh(); x2=x+l+bw()-1; y2=y+h-bh()-1; }
  else { x1=x+bw(); y1=y+h; x2=x+l-bw(); y2=y+h+bh()-1; }
}

int scroller::bh() { if (vert) return 15; else return 13; }
int scroller::bw() { if (vert) return 12; else return 14; }

uint8_t *scroller::b1()
{
  if (vert) return vs_up_arrow;
  else return hs_left_arrow;
}

uint8_t *scroller::b2()
{
  if (vert) return vs_down_arrow;
  else return hs_right_arrow;
}


void scroller::draw_first(image *screen)
{
  if (sx>=t) sx=t-1;
  draw(0,screen);
  screen->widget_bar(b1x(),b1y(),b1x()+bw()-1,b1y()+bh()-1,
            wm->bright_color(),wm->medium_color(),wm->dark_color());
  screen->widget_bar(b2x(),b2y(),b2x()+bw()-1,b2y()+bh()-1,
            wm->bright_color(),wm->medium_color(),wm->dark_color());
  show_icon(screen,b1x()+2,b1y()+2,bw()-4,bh()-4,b1());
  show_icon(screen,b2x()+2,b2y()+2,bw()-4,bh()-4,b2());

  int x1,y1,x2,y2;
  dragger_area(x1,y1,x2,y2);
  screen->bar(x1,y1,x2,y2,wm->black());
  screen->bar(x1+1,y1+1,x2-1,y2-1,wm->medium_color());
  draw_widget(screen,0);
  scroll_event(sx,screen);
}

void scroller::wig_area(int &x1, int &y1, int &x2, int &y2)
{
  int sx1,sy1,sx2,sy2;
  dragger_area(sx1,sy1,sx2,sy2);
  if (vert)
  {
    x1=x+l+1;
    if (t<2)
      y1=y+bh()+1;
    else
      y1=y+bh()+1+sx*(sy2-sy1+1-bh())/(t-1);
  } else
  {
    if (t<2) x1=x+bw()+1;
    else x1=x+bw()+1+sx*(sx2-sx1+1-bw())/(t-1);
    y1=y+h+1;
  }
  x2=x1+bw()-3;
  y2=y1+bh()-3;

}


void scroller::draw_widget(image *screen, int erase)
{
  int x1,y1,x2,y2;
  wig_area(x1,y1,x2,y2);
  if (erase)
    screen->bar(x1,y1,x2,y2,wm->medium_color());
  else
    screen->widget_bar(x1,y1,x2,y2,
              wm->bright_color(),wm->medium_color(),wm->dark_color());
}

void scroller::draw(int active, image *screen)
{
  int x1,y1,x2,y2;
  area(x1,y1,x2,y2);
  screen->rectangle(x1,y1,x2,y2,active ? wm->bright_color() : wm->dark_color());
}


void scroller::handle_event(event &ev, image *screen, InputManager *inm)
{
  int mx=ev.mouse_move.x,my=ev.mouse_move.y;
  switch (ev.type)
  {
    case EV_MOUSE_BUTTON :
    {
      if (ev.mouse_button && drag==-1)
      {
    if (mx>=b1x() && mx<b1x()+bw() && my>=b1y()-2 && my<b1y()+bh())
    {
      if (sx>0)
      {
        draw_widget(screen,1);
        sx--;
        draw_widget(screen,0);
        scroll_event(sx,screen);
      }
    } else if (mx>=b2x() && mx<b2x()+bw() && my>=b2y() && my<=b2y()+bh())
    {
      if (sx<t-1)
      {
        draw_widget(screen,1);
        sx++;
        draw_widget(screen,0);
        scroll_event(sx,screen);
      }
    }
    else
    {
      int dx1,dy1,dx2,dy2;
      dragger_area(dx1,dy1,dx2,dy2);
      if (mx>=dx1 && mx<=dx2 && my>=dy1 && my<=dy2)
      {
        int x1,y1,x2,y2;
        wig_area(x1,y1,x2,y2);
        if (mx>=x1 && mx<=x2 && my>=y1 && my<=y2)
        {
          drag=sx;
          inm->grab_focus(this);
        }
        else if (t>1)
        {
          int nx=mouse_to_drag(mx,my);
          if (nx!=sx && nx>=0 && nx<t)
          {
        draw_widget(screen,1);
        sx=nx;
        draw_widget(screen,0);
        scroll_event(sx,screen);
          }
        }
      } else handle_inside_event(ev,screen,inm);
    }
      } else if (!ev.mouse_button && drag!=-1)
      {
    inm->release_focus();
    drag=-1;
      }
    } break;

    case EV_MOUSE_MOVE :
    {
      if (drag!=-1)
      {
    int nx=mouse_to_drag(mx,my);
    if (nx<0) nx=0; else if (nx>=t) nx=t-1;
    if (nx!=sx)
    {
      draw_widget(screen,1);
      sx=nx;
      draw_widget(screen,0);
      scroll_event(sx,screen);
    }
      } else if ( activate_on_mouse_move())
      {
    int x1,y1,x2,y2;
    wig_area(x1,y1,x2,y2);
    if (mx>=x && mx<=x+l-1 && my>=y && my<=y+h-1)
      handle_inside_event(ev,screen,inm);
      }

    } break;
    case EV_KEY :
    {
      switch (ev.key)
      {
    case JK_LEFT :
    { handle_left(screen,inm); } break;
    case JK_RIGHT :
    { handle_right(screen,inm); } break;
    case JK_UP :
    { handle_up(screen,inm); } break;
    case JK_DOWN :
    { handle_down(screen,inm); } break;

    default :
      handle_inside_event(ev,screen,inm);
      }
    } break;
  }
}


void scroller::handle_right(image *screen, InputManager *inm)
{
  if (!vert && sx<t-1)
  {
    draw_widget(screen,1);
    sx++;
    draw_widget(screen,0);
    scroll_event(sx,screen);
  }
}

void scroller::handle_left(image *screen, InputManager *inm)
{
  if (!vert && sx>1)
  {
    draw_widget(screen,1);
    sx--;
    draw_widget(screen,0);
    scroll_event(sx,screen);
  }
}

void scroller::handle_up(image *screen, InputManager *inm)
{
  if (vert && sx>1)
  {
    draw_widget(screen,1);
    sx--;
    draw_widget(screen,0);
    scroll_event(sx,screen);
  }
}

void scroller::handle_down(image *screen, InputManager *inm)
{
  if (vert && sx<t-1)
  {
    draw_widget(screen,1);
    sx++;
    draw_widget(screen,0);
    scroll_event(sx,screen);
  }
}

void scroller::set_x (int x, image *screen)
{
  if (x<0) x=0;
  if (x>=t) x=t-1;
  if (x!=sx)
  {
    draw_widget(screen,1);
    sx=x;
    draw_widget(screen,0);
    scroll_event(sx,screen);
  }
}

int scroller::mouse_to_drag(int mx,int my)
{
  int x1,y1,x2,y2;
  dragger_area(x1,y1,x2,y2);

  if (vert)
  {
    int h=(y2-y1+1-bh());
    if (h)
      return (my-y-bh()-bh()/2)*(t-1)/h;
    else return 0;
  }
  else
  {
    int w=(x2-x1+1-bw());
    if (w)
      return (mx-x-bw()-bw()/2)*(t-1)/w;
    else return 0;
  }
}


void scroller::scroll_event(int newx, image *screen)
{
  screen->bar(x,y,x+l-1,y+h-1,wm->black());
  int xa,ya,xo=0,yo;
  if (vert) { xa=0; ya=30; yo=x+5; yo=y+5; } else { xa=30; ya=0; xo=x+5; yo=y+5; }
  for (int i=newx,c=0; c<30 && i<100; i++,c++)
  {
    char st[10];
    sprintf(st,"%d",i);
    wm->font()->put_string(screen,xo,yo,st,wm->bright_color());
    xo+=xa; yo+=ya;
  }
}

void pick_list::area_config()
{
  l=wid*wm->font()->width();
  h=th*(wm->font()->height()+1);
}

int lis_sort(void const *a, void const *b)
{
  pick_list_item *a1=(pick_list_item *)a;
  pick_list_item *a2=(pick_list_item *)b;
  return strcmp(a1->name,a2->name);
}

pick_list::pick_list(int X, int Y, int ID, int height,
        char **List, int num_entries, int start_yoffset, ifield *Next, image *texture)
     : scroller(X,Y,ID,2,2,1,0,Next)
{
  th=height;
  tex=texture;
  t=num_entries;
  wid=0;
  key_hist_total=0;
  lis=(pick_list_item *)malloc(sizeof(pick_list_item)*num_entries);
  int i=0;
  for (; i<num_entries; i++)
  {
    lis[i].name=List[i];
    lis[i].number=i;
  }
  qsort((void *)lis,num_entries,sizeof(pick_list_item),lis_sort);

  for (i=0; i<t; i++)
    if ((int)strlen(List[i])>wid)
      wid=strlen(List[i]);
  cur_sel=sx=start_yoffset;
}

void pick_list::handle_inside_event(event &ev, image *screen, InputManager *inm)
{
  if (ev.type==EV_MOUSE_MOVE && activate_on_mouse_move())
  {
    int sel=last_sel+(ev.mouse_move.y-y)/(wm->font()->height()+1);
    if (sel!=cur_sel && sel<t && sel>=0)
    {
      cur_sel=sel;
      scroll_event(last_sel,screen);
    }
  }
  else if (ev.type==EV_MOUSE_BUTTON)
  {
    int sel=last_sel+(ev.mouse_move.y-y)/(wm->font()->height()+1);
    if (sel<t && sel>=0)
    {
      if (sel==cur_sel)
      wm->push_event(new event(id,(char *)this));
      else
      {
    cur_sel=sel;
    scroll_event(last_sel,screen);
      }
    }
  } else if (ev.type==EV_KEY && ev.key==JK_ENTER)
    wm->push_event(new event(id,(char *)this));
  else if (ev.type==EV_KEY)
  {
    int found=-1;
    if (key_hist_total<20)
      key_hist[(int)(key_hist_total++)]=ev.key;

    for (int i=0; i<t && found==-1; i++)
    {
      if ((int)strlen(lis[i].name)>=key_hist_total && memcmp(lis[i].name,key_hist,key_hist_total)==0)
    found=i;
    }
    if (found!=-1)
    {
      sx=found;
      cur_sel=found;
      scroll_event(sx,screen);
    } else key_hist_total=0;
  }
}

void pick_list::handle_up(image *screen, InputManager *inm)
{
  if (cur_sel>0)
    cur_sel--;
  else return ;
  if (cur_sel<sx)
  {
    draw_widget(screen,1);
    sx=cur_sel;
    draw_widget(screen,0);
  }
  scroll_event(sx,screen);
}

void pick_list::handle_down(image *screen, InputManager *inm)
{
  if (cur_sel<t-1)
    cur_sel++;
  else return ;
  if (cur_sel>sx+th-1)
  {
    draw_widget(screen,1);
    sx=cur_sel-th+1;
    draw_widget(screen,0);
  }
  scroll_event(sx,screen);
}

void pick_list::scroll_event(int newx, image *screen)
{
  last_sel=newx;
  if (tex)
  {
    int cx1, cy1, cx2, cy2;
    screen->GetClip(cx1, cy1, cx2, cy2);
    screen->SetClip(x,y,x+l,y+h);
    int tw=(l+tex->Size().x-1)/tex->Size().x;
    int th=(h+tex->Size().y-1)/tex->Size().y;
    int dy=y;
    for (int j=0; j<th; j++,dy+=tex->Size().y)
      for (int i=0,dx=x; i<tw; i++,dx+=tex->Size().x)
        tex->put_image(screen,dx,dy);

    screen->SetClip(cx1, cy1, cx2, cy2);
  } else screen->bar(x,y,x+l-1,y+h-1,wm->black());

  int dy=y;
  for (int i=0; i<th; i++,dy+=wm->font()->height()+1)
  {
    if (i+newx==cur_sel)
      screen->bar(x,dy,x+wid*wm->font()->width()-1,dy+wm->font()->height(),wm->dark_color());
    if (i+newx<t)
      wm->font()->put_string(screen,x,dy,lis[i+newx].name,wm->bright_color());
  }
}




spicker::spicker(int X, int Y, int ID, int Rows, int Cols, int Vert, int MultiSelect,
           ifield *Next)
     : scroller(X,Y,ID,2,2,Vert,0,Next)
{
  l=-1;
  last_click=-1;
  r=Rows;
  c=Cols;
  m=MultiSelect;
  select=NULL;
}

void spicker::set_select(int x, int on)
{
  if (m)
  {
    if (on)
      select[x/8]|=1<<(x&7);
    else
      select[x/8]&=(0xff-(1<<(x&7)));
  } else cur_sel=x;
}

int spicker::get_select(int x)
{
  if (m)
    return select[x/8]&(1<<(x&7));
  else return (x==cur_sel);
}

int spicker::first_selected()
{
  if (m)
  {
    for (int i=0; i<t; i++)
      if (get_select(i)) return i;
    return -1;
  } else return cur_sel;
}

void spicker::reconfigure()
{
  if (select)
    free(select);
  select=NULL;


  t=total();
  if (sx>t)
    sx=t-1;
  if (m)
  {
    select=(uint8_t *)malloc((t+7)/8);
    memset(select,0,(t+7)/8);
  } else cur_sel=0;
}

void spicker::draw_background(image *screen)
{
  screen->bar(x,y,x+l-1,y+h-1,wm->dark_color());
}


void spicker::area_config()
{
  if (vert)
    l=item_width()+4;
  else
    l=item_width()*c+4;

  if (vert)
    h=item_height()*r+4;
  else
    h=item_height()+4;

}

void spicker::set_x(int x, image *screen)
{
  cur_sel=x;
  sx=x;
  scroll_event(x,screen);
}


void spicker::scroll_event(int newx, image *screen)
{
  last_sel=newx;
  int xa,ya,xo,yo;
  xo=x+2;
  yo=y+2;
  if (vert) { xa=0; ya=item_height(); }
  else { xa=item_width(); ya=0; }
  draw_background(screen);

  for (int i=newx; i<newx+vis(); i++)
  {
    if (i<t)
    {
      if (m)
        draw_item(screen,xo,yo,i,get_select(i));
      else
        draw_item(screen,xo,yo,i,i==cur_sel);
    }
    xo+=xa; yo+=ya;
  }
}


void spicker::handle_inside_event(event &ev, image *screen, InputManager *inm)
{
  switch (ev.type)
  {
    case EV_MOUSE_MOVE :
    {
      if (activate_on_mouse_move())
      {
    int me;
    if (vert)
      me=last_sel+(ev.mouse_move.y-y)/item_height();
    else
      me=last_sel+(ev.mouse_move.x-x)/item_width();
    if (me<t && me>=0)
    {
      if (cur_sel!=me)
      {
        cur_sel=me;
        scroll_event(last_sel,screen);
        note_new_current(screen,inm,me);
      }
    }
      }
    } break;
    case EV_MOUSE_BUTTON :
    {
      int me;
      if (vert)
    me=last_sel+(ev.mouse_move.y-y)/item_height();
      else
    me=last_sel+(ev.mouse_move.x-x)/item_width();
      if (me<t && me>=0)
      {
    if (m)
    {
      if (ev.mouse_button)
      {
        if (ok_to_select(me))
        {
          set_select(me,!get_select(me));
          scroll_event(last_sel,screen);
          inm->grab_focus(this);
        }
      } else last_click=-1;

    } else if (ok_to_select(me))
    {
      if (cur_sel==me)
        note_selection(screen,inm,me);
      else
      {
        cur_sel=me;
        scroll_event(last_sel,screen);
        note_new_current(screen,inm,me);
      }
    }
      }
    } break;
  }
}



void spicker::handle_up(image *screen, InputManager *inm)
{
  if (vert && cur_sel>0)
  {
    cur_sel--;

    if (cur_sel<sx)
    {
      draw_widget(screen,1);
      last_sel=sx=cur_sel;
      draw_widget(screen,0);
    }
    scroll_event(last_sel,screen);
    note_new_current(screen,inm,cur_sel);
  }
}

void spicker::handle_down(image *screen, InputManager *inm)
{
  if (vert && cur_sel<t-1)
    cur_sel++;
  else return ;
  if (cur_sel>sx+r-1)
  {
    draw_widget(screen,1);
    last_sel=sx=cur_sel-r+1;
    draw_widget(screen,0);
  }
  scroll_event(sx,screen);
  note_new_current(screen,inm,cur_sel);
}

void spicker::handle_left(image *screen, InputManager *inm)
{
}

void spicker::handle_right(image *screen, InputManager *inm)
{
}




