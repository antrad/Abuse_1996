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

#include "particle.h"
#include "view.h"
#include "lisp.h"
#include "cache.h"
#include "jrand.h"


static int total_pseqs=0;
static part_sequence **pseqs=NULL;
static part_animation *first_anim=NULL,*last_anim=NULL;

void free_pframes()
{
  for (int i=0; i<total_pseqs; i++)
    delete pseqs[i];
  if (total_pseqs)
    free(pseqs);
}

part_frame::~part_frame()
{
  free(data);
}

void add_panim(int id, long x, long y, int dir)
{
  CONDITION(id>=0 && id<total_pseqs,"bad id for particle animation");
  part_animation *pan=new part_animation(pseqs[id],x,y,dir,NULL);
  if (!first_anim)
    first_anim=last_anim=pan;
  else
  {
    last_anim->next=pan;
    last_anim=pan;
  }
}

void delete_panims()
{
  while (first_anim)
  {
    last_anim=first_anim;
    first_anim=first_anim->next;
    delete last_anim;
  }
  last_anim=NULL;
}

int defun_pseq(void *args)
{
  LSymbol *sym=(LSymbol *)lcar(args);
  if (item_type(sym)!=L_SYMBOL)
  {
    ((LObject *)args)->Print();
    printf("expecting first arg to def-particle to be a symbol!\n");
    exit(0);
  }
  LSpace *sp = LSpace::Current;
  LSpace::Current = &LSpace::Perm;
  sym->SetNumber(total_pseqs); // set the symbol value to the object number
  LSpace::Current = sp;
  pseqs=(part_sequence **)realloc(pseqs,sizeof(part_sequence *)*(total_pseqs+1));

  args=lcdr(args);
  pseqs[total_pseqs]=new part_sequence(args);
  total_pseqs++;
  return total_pseqs;
}

extern int total_files_open;

part_sequence::part_sequence(void *args)
{
  char *fn=lstring_value(lcar(args));
  bFILE *fp=open_file(fn,"rb");
  if (fp->open_failure())
  {
    delete fp;
    ((LObject *)args)->Print();
    fprintf(stderr,"\nparticle sequence : Unable to open %s for reading\n",fn);
    fprintf(stderr,"total files open=%d\n",total_files_open);

    FILE *fp=fopen(fn,"rb");
    printf("convet = %d\n",fp!=NULL);
    exit(1);
  }

  // count how many frames are in the file
  spec_directory sd(fp);
  delete fp;
  tframes=0;
  int i=0;
  for (; i<sd.total; i++)
    if (sd.entries[i]->type==SPEC_PARTICLE) tframes++;
  frames=(int *)malloc(sizeof(int)*tframes);

  int on=0;
  for (i=0; i<sd.total; i++)
    if (sd.entries[i]->type==SPEC_PARTICLE)
      frames[on++]=cache.reg(fn,sd.entries[i]->name,SPEC_PARTICLE,1);

}

part_frame::part_frame(bFILE *fp)
{
  t=fp->read_uint32();
  data=(part *)malloc(sizeof(part)*t);
  x1=y1=100000; x2=y2=-100000;
  for (int i=0; i<t; i++)
  {
    int16_t x=fp->read_uint16();
    int16_t y=fp->read_uint16();
    if (x<x1) x1=x;
    if (y<y1) y1=y;
    if (x>x2) x2=x;
    if (y>y2) y2=x;
    data[i].x=x;
    data[i].y=y;
    data[i].color=fp->read_uint8();
  }
}

void tick_panims()
{
  part_animation *last=NULL;
  for (part_animation *p=first_anim; p; )
  {
    p->frame++;
    if (p->frame>=p->seq->tframes)
    {
      if (last)
        last->next=p->next;
      else first_anim=first_anim->next;
      if (last_anim==p) last_anim=last;
      part_animation *d=p;
      p=p->next;
      delete d;
    } else
    {
      last=p;
      p=p->next;
    }

  }
}

void draw_panims(view *v)
{
  for (part_animation *p=first_anim; p; p=p->next)
  {
    cache.part(p->seq->frames[p->frame])->draw(main_screen,p->x-v->xoff()+v->m_aa.x,p->y-v->yoff()+v->m_aa.y,p->dir);
  }
}

void part_frame::draw(image *screen, int x, int y, int dir)
{
    ivec2 caa, cbb;
    screen->GetClip(caa, cbb);

    if (x + x1 >= cbb.x || x + x2 < caa.x || y + y1 >= cbb.y || y + y2 < caa.y)
       return;

  part *pon=data;
  caa.y -= y;
  cbb.y -= y;

  int i=t;
  while (i && pon->y<caa.y) { pon++; i--; }
  if (!i) return ;
  screen->Lock();
  if (dir>0)
  {
    while (i && pon->y < cbb.y)
    {
      long dx=x-pon->x;
      if (dx >= caa.x && dx < cbb.x)
      *(screen->scan_line(pon->y+y)+dx)=pon->color;
      i--;
      pon++;
    }
  } else
  {
    while (i && pon->y < cbb.y)
    {
      long dx=pon->x+x;
      if (dx >= caa.x && dx < cbb.x)
        *(screen->scan_line(pon->y+y)+dx)=pon->color;
      i--;
      pon++;
    }
  }
  screen->Unlock();
}

void ScatterLine(ivec2 p1, ivec2 p2, int c, int s)
{
    ivec2 caa, cbb;
    main_screen->GetClip(caa, cbb);

    int t = 1 + Max(abs(p2.x - p1.x), abs(p2.y - p1.y));
    int xo = p1.x << 16,
        yo = p1.y << 16,
        dx = ((p2.x - p1.x) << 16) / t,
        dy = ((p2.y - p1.y) << 16) / t;

    int xm = (1 << s);
    int ym = (1 << s);
    s = (15 - s);

    main_screen->Lock();
    while(t--)
    {
        int x = (xo >> 16) + (jrand() >> s) - xm;
        int y = (yo >> 16) + (jrand() >> s) - ym;
        if(!(x < caa.x || y < caa.y || x >= cbb.x || y >= cbb.y))
        {
            *(main_screen->scan_line(y) + x) = c;
        }
        xo += dx;
        yo += dy;
    }
    main_screen->Unlock();
}

void AScatterLine(ivec2 p1, ivec2 p2, int c1, int c2, int s)
{
    ivec2 caa, cbb;
    main_screen->GetClip(caa, cbb);

    int t = 1 + Max(abs(p2.x - p1.x), abs(p2.y - p1.y));
    int xo = p1.x << 16,
        yo = p1.y << 16,
        dx = ((p2.x - p1.x) << 16) / t,
        dy = ((p2.y - p1.y) << 16) / t;

    int xm = (1 << s);
    int ym = (1 << s);
    s = (15 - s);

    main_screen->Lock();

    int w = main_screen->Size().x;
    uint8_t *addr;

    while(t--)
    {
        int x = (xo >> 16) + (jrand() >> s) - xm;
        int y = (yo >> 16) + (jrand() >> s) - ym;
        // FIXME: these clip values seemed wrong to me before the GetClip
        // refactoring.
        if(!(x <= caa.x || y <= caa.y || x >= cbb.x - 1 || y >= cbb.y - 1))
        {
            addr = main_screen->scan_line(y) + x;
            *addr = c1;
            *(addr + w) = c2;
            *(addr - w) = c2;
            *(addr - 1) = c2;
            *(addr + 1) = c2;
        }
        xo += dx;
        yo += dy;
    }

    main_screen->Unlock();
}

