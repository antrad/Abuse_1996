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

#include <math.h>

#include "common.h"

#include "palette.h"
#include "image.h"
#include "video.h"
#include "filter.h"

palette *lastl=NULL;

palette::palette(bFILE *fp)
{
  ncolors=fp->read_uint16();
  pal=(color *)malloc(sizeof(color)*ncolors);
  usd=(unsigned char *)malloc(ncolors/8+1);
  set_all_unused();
  fp->read(pal,sizeof(color)*ncolors);
  bg=0;
}

palette::palette(spec_entry *e, bFILE *fp)
{
  fp->seek(e->offset,0);
  ncolors=fp->read_uint16();
  pal=(color *)malloc(sizeof(color)*ncolors);
  usd=(unsigned char *)malloc(ncolors/8+1);
  set_all_unused();
  fp->read(pal,sizeof(color)*ncolors);
  bg=0;
}

int palette::size()
{
  return ncolors*sizeof(color)+2;
}

int palette::write(bFILE *fp)
{
  fp->write_uint16(ncolors);
  return fp->write(pal,sizeof(color)*ncolors)==ncolors;
}

int palette::find_closest(uint8_t r, uint8_t g, uint8_t b)
{
   unsigned char *cl=(unsigned char *)addr();
   int c=0,d=0x100000,i,nd;
   for (i=0; i<256; i++)
   {
     nd=((int)r-(int)(*cl))*((int)r-(int)(*cl)); cl++;
     nd+=((int)g-(int)(*cl))*((int)g-(int)(*cl)); cl++;
     nd+=((int)b-(int)(*cl))*((int)b-(int)(*cl)); cl++;
     if (nd<d)
     { c=i; d=nd; }
   }
   return c;
}

int palette::find_color(uint8_t r, uint8_t g, uint8_t b)
{
  int i,ub,mask,find;
  for (i=0,ub=0,mask=128,find=-1; i<ncolors && find<0; i++)
  {
    if (usd[ub]&mask)
      if (r==pal[i].red && b==pal[i].blue && g==pal[i].green)
    find=i;
    mask>>=1;
    if (mask==0)
    { mask=128; ub++; }
  }
  return find;
}

uint32_t palette::getquad(int x)
{ union { char entry[4]; uint32_t ret; };
  entry[3]=0;
  entry[2]=pal[x].red;
  entry[1]=pal[x].green;
  entry[0]=pal[x].blue;
  return ret;
}


void palette::black_white()
{
  int i;
  unsigned char r,g,b,gr;

  for (i=0; i<256; i++)
  {
    get(i,r,g,b);
    gr=(unsigned char)((double) r*0.30+(double) g*0.59+(double)b*0.11);
    set(i,gr,gr,gr);
  }
}

void palette::make_black_white()
{
  int i,c;
  set(0,0,0,0);
  for (i=1; i<ncolors; i++)
  { c=(int)((double)i/(double)ncolors*(double)255);
    set(i,c,c,c);
  }
}

void palette::set_rgbs()
{
  int i,v;
  CHECK(ncolors==256);
  for (i=0; i<64; i++)
  {
    if (i==0) v=0;
    else
    {
      v=(int) ((double)i+(double)(sqrt(63.0-i)));
      v<<=2;
    }

    set(i,         i,     0,     0);            // reds 0-63
    set(i+64,      0,     i,     0);
    set(i+128,     0,     0,     i);       // blues 128-191
    set(i+128+64,  v,     v,     v);        // whites .. rest
  }
  set_all_used();
}

void palette::set_all_used()
{
  int i;
  for (i=0; i<ncolors; i++) set_used(i);
}

void palette::set_all_unused()
{
  int i;
  for (i=0; i<ncolors; i++) set_unused(i);
}


palette *palette::copy()
{
  palette *p;
  int i;
  p=new palette(ncolors);
  for (i=0; i<ncolors; i++)
  {
    if (used(i))
      p->set_used(i);
    else p->set_unused(i);
    p->set(i,red(i),green(i),blue(i));
  }
  return p;
}

void palette::set_used(int color_num)
{
  int x,b;
  CHECK(color_num>=0 && color_num<ncolors);
  x=color_num/8;
  b=color_num%8;
  usd[x]|=(128>>b);
}

void palette::set_unused(int color_num)
{
  int x,b;
  CHECK(color_num>=0 && color_num<ncolors);
  x=color_num/8;
  b=color_num%8;
  usd[x]&=(0xff^(128>>b));
}

int palette::used(int color_num)
{
  int x,b;
  CHECK(color_num>=0 && color_num<ncolors);
  x=color_num/8;
  b=color_num%8;
  return (usd[x]&(128>>b));
}

void palette::defaults()
{
  int i;
  set(0,0,0,0);
  set_used(0);
  for (i=1; i<ncolors; i++)
    set_unused(i);
  if (ncolors==256)
    for (i=0; i<ncolors; i++)
      set(i,RED3(i),GREEN3(i),BLUE2(i));
  else if (ncolors==16)
    for (i=0; i<ncolors; i++)
      set(i,255-(i&3),255-((i&4)>>2),255-((i&8)>>3));
  else
    for (i=0; i<ncolors; i++)
      set(i,255-(i%3),255-((i+1)%3),255-((i+2)%3));
}

void palette::shift(int amount)
{
  int i;
  unsigned char m;
  if (amount<0)
  {

    m=-amount;
    for (i=0; i<ncolors*3; i++)
      ((unsigned char *) pal)[i]>>=m;
  }
  else if (amount>0)
  {
    m=amount;
    for (i=0; i<ncolors*3; i++)
      ((unsigned char *) pal)[i]<<=m;
  }
}



void palette::set(int x, unsigned char red, char unsigned green, char unsigned blue)
{ CONDITION(x>=0 && x<ncolors,"Pallete::set passed bad x");
  CONDITION((int)red<=ncolors && (int)green<=ncolors && (int)blue<=ncolors,
        "pallette::set color values bigger than palette");
  pal[x].red=red; pal[x].green=green; pal[x].blue=blue;
}

void palette::get(int x, unsigned char &red, unsigned char &green, unsigned char &blue)
{ CONDITION(x>=0 && x<ncolors,"Pallete::get passed bad x");
  red=pal[x].red; green=pal[x].green; blue=pal[x].blue;
}
palette::~palette()
{ if (pal) free(pal);
  if (usd) free(usd);
}

palette::palette(int number_colors)
{
  CONDITION(number_colors>0,"palette::constructor - need at least one color!");
  ncolors=number_colors;
  bg=0;
  pal=(color *)malloc(ncolors*3);
  usd=(unsigned char *)malloc(ncolors/8+1);
  defaults();
}



quant_node::~quant_node()
{
/*  if (!is_leaf())
  { for (i=0; i<8; i++)
      if (children[i])
      {    delete children[i];
    children[i]=NULL;
      }
  } */
}


/*void quant_node::prune()
{
  int t,r,g,b;
  CONDITION(!is_leaf(),"Cannot prune a leaf!");
  total(t,r,g,b);
  red=r/t;
  green=g/t;
  blue=b/t;
  be_childish();
} */

void quant_node::total(int &tnodes, int &tr, int &tg, int &tb)
{
  int i;
  if (is_leaf())
  { tnodes+=tot;
    tr+=red*tot;
    tg+=green*tot;
    tb+=blue*tot;
  }
  else
  { for (i=0; i<8; i++)
      if (children[i])
    children[i]->total(tnodes,tr,tg,tb);
  }
}

quant_node::quant_node(int level, quant_node *dad,
    unsigned char r, unsigned char g, unsigned char b)
{
  int i;
  CONDITION(level<=8,"Tree cannot have more than eight levels");
  if (level==8)
    be_childish();
  else
    for (i=0; i<8; i++) children[i]=NULL;
  padre=dad;
  red=r; green=g; blue=b;
  tot=0;
}

quant_palette::quant_palette(int max_colors)
{ root=NULL; nc=0; mx=max_colors; }

void quant_palette::re_delete(quant_node *who, int lev)  // removes all children from memory
{ int x;                                  // and recurses down
  if (who)
  {
    if (!who->is_leaf())
    { for (x=0; x<8; x++)
    if (who->children[x])
    {
      CONDITION(lev<8,"Levl > 7");
      re_delete(who->children[x],lev+1);
      level[lev].unlink(who->children[x]);
      delete who->children[x];
    }
    }
  }
}

void quant_palette::prune()
{
  int pruned,lev,x,r,g,b,t;
  quant_node *p=NULL,*f=NULL;
  for (pruned=0,lev=8; lev>1 && !pruned; lev--)
  {
    p=(quant_node *)level[lev-1].first();
    if (p)
    { do
      {
    f=p->father();
    for (x=0; x<8 && !pruned; x++)
      if (f->children[x])
        if (f->children[x]->Next()!=p->Next())        // if this son is not me!
        pruned=1;                   //  I have a brother! stop
       p=(quant_node *)p->Next();
      } while (p != level[lev-1].first() && !pruned);
    }
  }
  CONDITION(lev>0,"could not prune!");
  t=0; r=0; g=0; b=0;
  f->total(t,r,g,b);
  if (t<=1)
  {
    t=0; r=0; g=0; b=0;
    f->total(t,r,g,b);
  }
  CONDITION(t>1,"Should be more colors\n");
  printf("%d Pruned at level %d, r=%d, g=%d, b=%d, total nodes off = %d\n",nc,
    lev,r/t,g/t,b/t,t);
  f->set(r/t,g/t,b/t);
  nc-=t;
  nc++;
  re_delete(f,lev);
  f->be_childish();
}

quant_palette::~quant_palette()
{
  if (root)
  {
    re_delete(root,1);
    delete root;
  }
}

uint8_t palette::brightest(int all)
{ uint8_t r,g,b,bri;
  unsigned i;
  long brv;
  brv=0; bri=0;

  for (i=0; i<(unsigned int)ncolors; i++)
  { if (all || used(i))
    {
      get(i,r,g,b);
      if ((long)r*(long)g*(long)b>brv)
      { brv=(long)r*(long)g*(long)b;
    bri=i;
      }
    }
  }

  return bri;
}

uint8_t palette::darkest(int all, int noblack)
{ uint8_t r,g,b,bri;
  unsigned i;
  long brv,x;
  brv=(long)258*(long)258*(long)258; bri=0;

  for (i=0; i<(unsigned int)ncolors; i++)
  { if (all || used(i))
    {
      get(i,r,g,b);
      x=(long)r*(long)g*(long)b;
      if (x<brv && (x || !noblack))
      { brv=(long)r*(long)g*(long)b;
    bri=i;
      }
    }
  }
  return bri;
}



palette *last_loaded()
{ return lastl; }

void palette::fade_to(int total_fades, int fade_on, int dest_r, int dest_g, int dest_b)
{
  uint8_t *sl=(uint8_t *)addr();
  uint8_t x;
  int i;
  for (i=0; i<ncolors; i++)
  {
    x=(( dest_r-(int)*sl)*fade_on/total_fades+*sl);
    *(sl++)=x;
    x=(( dest_g-(int)*sl)*fade_on/total_fades+*sl);
    *(sl++)=x;
    x=(( dest_b-(int)*sl)*fade_on/total_fades+*sl);
    *(sl++)=x;
  }
}
