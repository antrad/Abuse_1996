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

#include "supmorph.h"
#include "specs.h"
#include "transimage.h"
#include "timing.h"
#include "filter.h"
#include "video.h"
#include "jrand.h"

#define p_swap(x,y) { x^=y; y^=x; x^=y; }
#define p_dist(x1,y1,x2,y2) (((int)(x1)-(int)x2)*((int)(x1)-(int)x2)+      \
                             ((int)(y1)-(int)y2)*((int)(y1)-(int)y2))

super_morph::super_morph(TransImage *hint1, TransImage *hint2,
             int aneal_steps, void (*stat_fun)(int))
{
  int x,y,w1=hint1->Size().x,
          h1=hint1->Size().y,
          w2=hint2->Size().x,
          h2=hint2->Size().y;
  if (w1>w2) w=w1; else w=w2;
  if (h1>h2) h=h1; else h=h2;
  unsigned char *dp;

  /************************ count up the number of hints ****************************/
  unsigned short hints1[256],hints2[256];
  memset(hints1,0,256*2);
  memset(hints2,0,256*2);

  dp=hint1->Data();
  for (y=0; y<h1; y++)
  {
    x=0;
    while (x<w1)
    {
      x+=*dp;      // skip over space
      dp++;
      if (x<w1)
      {
    int rl=*(dp++);
    while (rl--) { hints1[*(dp++)]++; x++; }
      }
    }
  }

  // hint2 image2
  dp=hint2->Data();
  for (y=0; y<h2; y++)
  {
    x=0;
    while (x<w2)
    {
      x+=*dp;      // skip over space
      dp++;
      if (x<w2)
      {
    int rl=*(dp++);
    while (rl--) { hints2[*(dp++)]++; x++; }
      }
    }
  }


  /****************** now sum up hints and alloc memory for movers *************/
  unsigned short start1[256],start2[256],
                 totals[256],
                 start=0,*h1p=hints1,*h2p=hints2;
  unsigned char hint_color[256],total_hints=0;

  for (y=0; y<256; y++,h1p++,h2p++)
    if (*h1p)
    {
      if (*h2p==0)
      {
    t=0;
    return ;
      }
      start1[y]=start2[y]=start;       // specify start of hint range
      if (*h1p>*h2p)
        totals[y]=*h1p;
      else totals[y]=*h2p;
      start+=totals[y];
      hint_color[total_hints++]=y;
    }

  t=start;
  movers=(unsigned char *)malloc(t*4);


  /**************** Now scan the images again setup hints *********************/
  dp=hint1->Data();
  for (y=0; y<h1; y++)
  {
    x=0;
    while (x<w1)
    {
      x+=*dp;      // skip over space
      dp++;
      if (x<w1)
      {
    int rl=*(dp++);
    while (rl--)
    {
      int maddr=(start1[*(dp++)]++)*4;
      movers[(maddr++)]=x;
      movers[maddr]=y;
      x++;
    }
      }
    }
  }

  dp=hint2->Data();
  for (y=0; y<h2; y++)
  {
    x=0;
    while (x<w2)
    {
      x+=*dp;      // skip over space
      dp++;
      if (x<w2)
      {
    int rl=*(dp++);
    while (rl--)
    {
      int maddr=(start2[*(dp++)]++)*4+2;
      movers[(maddr++)]=x;
      movers[maddr]=y;
      x++;
    }
      }
    }
  }

  /********* if hint sizes don't match duplicate the smaller until sizes are equal **********/
  for (start=0,x=0; x<total_hints; x++)
  {
    y=hint_color[x];
    int dups;
    for (dp=movers+start1[y]*4,dups=totals[y]-hints1[y]; dups; dups--)
    {
      *dp=*(dp-4); dp++;      // copy previous x,y position
      *dp=*(dp-4); dp++;
      dp+=2;
    }
    start1[y]-=2*totals[y]-hints1[y];        // set the start back to the begining of hint range
  }

  for (start=0,x=0; x<total_hints; x++)
  {
    y=hint_color[x];
    int dups;
    for (dp=movers+start2[y]*4+2,dups=totals[y]-hints2[y]; dups; dups--)
    {
      *dp=*(dp-4); dp++;      // copy previous x,y position
      *dp=*(dp-4); dp++;
      dp+=2;
    }
    start2[y]-=hints2[y];        // set the start back to the begining of hint range
  }


  /******* Now apply simulated annealing to solve for a smaller total distance ********/
  int rand_on=0;
  for (y=0; y<aneal_steps; y++)
  {
    if (stat_fun)
      stat_fun(y);
    dp=movers;
    for (x=0; x<total_hints; x++)
    {
      int hc=hint_color[x];
      int a,z=totals[hc];
      unsigned char *range_start=dp;
      for (a=0; a<z; a++,dp+=4)
      {
    unsigned char *swap=range_start+(rtable[((rand_on++)&(RAND_TABLE_SIZE-1))]%z)*4;
    int d_old=p_dist(dp[0],dp[1],dp[2],dp[3])+p_dist(swap[0],swap[1],swap[2],swap[3]);
    int d_new=p_dist(dp[0],dp[1],swap[2],swap[3])+p_dist(swap[0],swap[1],dp[2],dp[3]);
    if (d_new<d_old)
    {
      unsigned char s;
      s=swap[2]; swap[2]=dp[2]; dp[2]=s;
      s=swap[3]; swap[3]=dp[3]; dp[3]=s;
    }
      }
    }
  }
}

smorph_player::smorph_player(super_morph *m, palette *pal, image *i1, image *i2, int frames, int dir)
{
  int i,x1,y1,x2,y2;
  unsigned char *d=m->movers,*paddr=(unsigned char *)pal->addr(),*pa;
  stepper *p;
  p=steps=(stepper *)malloc(sizeof(stepper)*m->t);
  f_left=frames;
  frames--;
  t=m->t;
  w=m->w; h=m->h;

  for (i=0; i<t; i++,p++)
  {
    x1=*(d++);
    y1=*(d++);
    x2=*(d++);
    y2=*(d++);

    unsigned char r1,g1,b1,r2,g2,b2;
    pa=paddr+(int)(*(i1->scan_line(y1)+x1))*3;
    r1=*(pa++);
    g1=*(pa++);
    b1=*(pa++);

    pa=paddr+(int)(*(i2->scan_line(y2)+x2))*3;
    r2=*(pa++);
    g2=*(pa++);
    b2=*(pa++);

    p->r=r1<<16;
    p->g=g1<<16;
    p->b=b1<<16;

    p->dr=(long)(((int)r2-(int)r1)<<16)/frames;
    p->dg=(long)(((int)g2-(int)g1)<<16)/frames;
    p->db=(long)(((int)b2-(int)b1)<<16)/frames;

    if (dir<0)
    {
      x1=w-x1-1;
      x2=w-x2-1;
    }
    p->dx=((x2-x1)<<16)/frames;
    p->dy=((y2-y1)<<16)/frames;
    p->x=x1<<16;
    p->y=y1<<16;
  }

  hole=(unsigned char *)malloc(w*h);
}





int smorph_player::show(image *screen, int x, int y, ColorFilter *fil, palette *pal,
            int blur_threshold)
{
    if (!f_left)
        return 0;

    int i,px,py,ix,iy;
    ivec2 caa, cbb;
    screen->GetClip(caa, cbb);
    screen->AddDirty(ivec2(x, y), ivec2(x + w, y + h));
    stepper *ss;
    memset(hole,0,w*h);
    unsigned char *paddr=(unsigned char *)pal->addr();
    for (ss=steps,i=0; i<t; i++,ss++)
    {
      ix=(ss->x>>(16));
      iy=(ss->y>>(16));
      px=ix+x;
      py=iy+y;
      if (px>=caa.x && px < cbb.x && py>=caa.y && py < cbb.y)
      {
        hole[ix+iy*w]=*(screen->scan_line(py)+px)=fil->Lookup(ss->r>>(19),
                                    ss->g>>(19),
                                    ss->b>>(19));
      }
      ss->x+=ss->dx;
      ss->y+=ss->dy;
      ss->r+=ss->dr;
      ss->g+=ss->dg;
      ss->b+=ss->db;
    }
    f_left--;
    if (!f_left)    // skip hole fills and smoothing on last frame
      return 1;

    unsigned char *ll=hole+1,*tl=hole+w+1,*nl=hole+w*2+1;
    for (iy=1; iy<h-1; iy++)    // now scan the for holes to fill
    {
      for (ix=1; ix<w-1; ix++,ll++,tl++,nl++)
      {
    if (x+ix>=caa.x && x+ix < cbb.x && y+iy>=caa.y && y+iy < cbb.y)
    {
      int t=0;
      unsigned char *pa;
      int r=0,g=0,b=0;
/*      if (*(tl-1)) t++;
      if (*(tl+1)) t++;
      if (*ll) t++;
      if (*nl) t++; */

      if (*(tl-1)) { t++; pa=paddr+(*(tl-1))*3; r+=*(pa++); g+=*(pa++); b+=*(pa++); }
      if (*(tl+1)) { t++; pa=paddr+(*(tl+1))*3; r+=*(pa++); g+=*(pa++); b+=*(pa++); }
      if (*(ll)) { t++; pa=paddr+(*ll)*3; r+=*(pa++); g+=*(pa++); b+=*(pa++); }
      if (*(nl)) { t++; pa=paddr+(*nl)*3; r+=*(pa++); g+=*(pa++); b+=*(pa++); }

      if (*tl)
      {
        if (t)
        {
          pa=paddr+(*tl)*3;
          r/=t; g/=t; b/=t;
          int dist=((int)(*pa)-r)*((int)(*pa)-r); pa++;
          dist+=((int)(*pa)-g)*((int)(*pa)-g); pa++;
          dist+=((int)(*pa)-b)*((int)(*pa)-b);
          if (dist>blur_threshold)
            *(tl)=*(screen->scan_line(y+iy)+x+ix)=fil->Lookup(r>>3,g>>3,b>>3);
        } else *(tl)=*(screen->scan_line(y+iy)+x+ix)=0; // kill single pixels
      }
      else if (t>=3)
        *(tl)=*(screen->scan_line(y+iy)+x+ix)=fil->Lookup((r/t)>>3,(g/t)>>3,(b/t)>>3);
    }
      }
      ll+=2;
      tl+=2;
      nl+=2;
    }
    return 1;
}




/*void free_up_memory() { printf("you're screwed\n"); }

main(int argc, char **argv)
{
  image_init();
  jrand_init();
  FILE *fp=fopen("art/mrphmask.spe","rb");
  spec_directory sd(fp);
  image *h1=new image(sd.find("20 h"),fp),
        *h2=new image(sd.find("1h"),fp),
        *i1=new image(sd.find("20"),fp),
        *i2=new image(sd.find("1"),fp);
  palette *pal=new palette(sd.find(SPEC_PALETTE),fp);
  color_filter *fil=new color_filter(sd.find(SPEC_COLOR_TABLE),fp);

  int steps=atoi(argv[1]);
  if (steps<2) steps=50;
  TransImage *hh1=new TransImage(h1,"hint1"),*hh2=new TransImage(h2,"hint2");

  time_marker time1;
  super_morph sm(hh1,hh2,steps);
  int frames=atoi(argv[2]);
  if (frames<2) frames=16;
  smorph_player sp(&sm,pal,i1,i2,frames,-1);

  time_marker time2;
  printf("time = %lf\n",time2.diff_time(&time1));

  set_mode(19,argc,argv);
  pal->load();
  i1->put_image(screen,30,30);
  update_dirty(screen);
  sleep(2);
  while (sp.show(screen,30,30,fil,pal))
  { update_dirty(screen);
    screen->bar(30,30,30+sp.w,30+sp.h,0);
  }
  sleep(2);
  close_graphics();
}*/


