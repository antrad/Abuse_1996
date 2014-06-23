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

#include "help.h"
#include "game.h"

int help_page=0;


#define HELP_PAGES 3

char *get_page(int x)
{
  char tmp[800],line[120];
  FILE *fp=fopen("help.txt","r");
  if (!fp)
    return strcpy((char *)malloc(40),"help.txt missing");
  else
  {
    tmp[0]=0;
    while (x)
    {
      fgets(line,120,fp);
      if (line[0]=='.' && line[1]=='n')
        x--;
      if (line[0]=='.' && line[1]=='e')
      {
    fclose(fp);
    return strcpy((char *)malloc(30),"missing page");
      }

    }
    do
    {
      fgets(line,120,fp);
      if (line[0]=='.')
      {
        fclose(fp);
        return strdup(tmp);
      }
      else strcat(tmp,line);
    } while (1);
  }
}


void make_help_page(int page, image *s)
{
  int x=0,y=0,fw=wm->font()->width(),fh=wm->font()->height(),ya;
  char *h,*ho,imname[30],*inp;
  h=ho=get_page(page);

  s->clear(28);      // light gray

  bFILE *fp=open_file("art/joy.spe","rb");
  if (fp->open_failure())
  {
    wm->font()->put_string(s,10,10,"Help file missing!");
    return ;

  }
  spec_directory sd(fp);
  image *im;


  while (*h)
  {
    ya=fh;
    while (*h && *h!='\n')
    {
      if (*h=='@')
      {
    h++;
    if (*h=='@')
    {
          wm->font()->put_char(s,x,y,*h);
      x+=fw;
      h++;
    }
        else
        {
      inp=imname;
      int center=0;
      if (*h=='#')
      {
        center=1;
        h++;
      }
      if (*h=='+')
      {
        h++;
        while (*h!='+')
        {
          *inp=*h;
          h++;
          inp++;
        }
        h++;
        *inp=0;
        x=atoi(imname);
        inp=imname;
      }


      while (*h!='@')
      {
        *inp=*h;
        h++;
        inp++;
      }
      *inp=0;
      h++;

      if (!sd.find(imname))
      {
        wm->font()->put_string(s,0,yres-10,"Missing image!");
        delete fp;
        free(ho);
        return ;
      }
      im=new image(sd.find(imname),fp);

      if (center)
      {
          im->put_image(s,x,y-im->height()/2,1);
          if (im->height()/2>=ya)
            ya=im->height()/2+1;
      }
      else
      {
          im->put_image(s,x,y,1);
          if (im->height()>=ya)
            ya=im->height()+1;
      }
      x+=im->width()+1;
      delete im;
    }
      } else if (*h=='`')
      {
    inp=imname;
    h+=2;
    while (*h!='`')
    {
      *inp=*h;
      h++;
      inp++;
    }
    h++;
    *inp=0;
    x=atoi(imname);
      }
      else
      {
    wm->font()->put_char(s,x,y,*h);
    x+=fw;
    h++;
      }
    }
    y+=ya;
    x=0;
    if (*h) h++;
  }
  free(ho);
  delete fp;
}


void show_help(int direction)
{
  int i;
  image *h=new image(screen->width(),screen->height(),NULL,2),*old_screen;

  if (direction>0)
    make_help_page(help_page,h);
  else
  {
    old_screen=screen;
    screen=h;
    the_game->draw();
    screen=old_screen;
  }


  int steps=8;
  int scroll_step=screen->height()/steps;
  int helpy=-screen->height()+scroll_step;

  for (i=0; i<steps; i++,helpy+=scroll_step)
  {
    screen->scroll(0,0,xres,yres,0,scroll_step);
    h->put_part(screen,0,helpy,0,0,xres,-helpy+scroll_step);
    wm->flush_screen();
  }
  delete h;
}






void help_handle_event(event &ev)
{
  if (ev.window!=NULL) return ;

  if (the_game->state!=HELP_STATE)
  {
    if (ev.type==EV_KEY && (ev.key=='h' || ev.key=='?'))
    {
      the_game->state=HELP_STATE;
      help_page=0;
      show_help(1);
    }
  } else if (ev.type==EV_KEY)
  {
    if (ev.key==JK_ESC || help_page==HELP_PAGES-1)
    {
      show_help(-1);
      the_game->state=RUN_STATE;
    }
    else
    {
      help_page++;
      show_help(1);
    }
  }
}
