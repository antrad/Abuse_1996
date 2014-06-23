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

#include <stdlib.h>

#include "light.h"
#include "image.h"
#include "video.h"
#include "palette.h"
#include "timing.h"
#include "specs.h"
#include "dprint.h"
#include "filter.h"
#include "video.h"

light_source *first_light_source=NULL;
unsigned char *white_light,*green_light;
unsigned short min_light_level=0;
extern int vmode;

int light_detail=MEDIUM_DETAIL;

long light_to_number(light_source *l)
{
    if( !l ) return 0;
    int x = 1;
    light_source *s;
    for (s = first_light_source; s; s = s->next, x++)
        if (s == l) return x;
    return 0;
}


light_source *number_to_light(long x)
{
    if (x == 0) return NULL;
    x--;
    light_source *s;
    for (s = first_light_source; x && s; x--, s = s->next);
    return s;
}

light_source *light_source::copy()
{
    next = new light_source(type,x,y,inner_radius,outer_radius,xshift,yshift,next);
    return next;
}

void delete_all_lights()
{
    while (first_light_source)
    {
        light_source *p = first_light_source;
        first_light_source = first_light_source->next;
        delete p;
    }
}

void delete_light(light_source *which)
{
  if (which==first_light_source)
  {
    first_light_source=first_light_source->next;
    delete which;
  }
  else
  {
    for (light_source *f=first_light_source; f->next!=which && f; f=f->next);
    if (f)
    {
      f->next=which->next;
      delete which;
    }
  }
}

void light_source::calc_range()
{
  switch (type)
  {
    case 0 :
    { x1=x-(outer_radius>>xshift); y1=y-(outer_radius>>yshift);
      x2=x+(outer_radius>>xshift); y2=y+(outer_radius>>yshift);
    } break;
    case 1 :
    { x1=x-(outer_radius>>xshift); y1=y-(outer_radius>>yshift);
      x2=x+(outer_radius>>xshift); y2=y;
    } break;
    case 2 :
    { x1=x-(outer_radius>>xshift); y1=y;
      x2=x+(outer_radius>>xshift); y2=y+(outer_radius>>yshift);
    } break;
    case 3 :
    { x1=x; y1=y-(outer_radius>>yshift);
      x2=x+(outer_radius>>xshift); y2=y+(outer_radius>>yshift);
    } break;
    case 4 :
    { x1=x-(outer_radius>>xshift); y1=y-(outer_radius>>yshift);
      x2=x; y2=y+(outer_radius>>yshift);
    } break;


    case 5 :
    { x1=x; y1=y-(outer_radius>>yshift);
      x2=x+(outer_radius>>xshift); y2=y;
    } break;
    case 6 :
    { x1=x-(outer_radius>>xshift); y1=y-(outer_radius>>yshift);
      x2=x; y2=y;
    } break;
    case 7 :
    { x1=x-(outer_radius>>xshift); y1=y;
      x2=x; y2=y+(outer_radius>>yshift);
    } break;
    case 8 :
    { x1=x; y1=y;
      x2=x+(outer_radius>>xshift); y2=y+(outer_radius>>yshift);
    } break;
    case 9 :
    {
      x1=x;
      y1=y;
      x2=x+xshift;
      y2=y+yshift;
    } break;

  }
  mul_div=(1<<16)/(outer_radius-inner_radius)*64;
}

light_source::light_source(char Type, long X, long Y, long Inner_radius,
               long Outer_radius, long Xshift,  long Yshift, light_source *Next)
{
  type=Type;
  x=X; y=Y;
  inner_radius=Inner_radius;
  outer_radius=Outer_radius;
  next=Next;
  known=0;
  xshift=Xshift;
  yshift=Yshift;
  calc_range();
}

light_source *add_light_source(char type, long x, long y,
                   long inner, long outer, long xshift, long yshift)
{
    first_light_source = new light_source(type,x,y,inner,outer,xshift,yshift,first_light_source);
    return first_light_source;
}


#define TTINTS 9
uchar *tints[TTINTS];

void calc_tint(uchar *tint, int rs, int gs, int bs, int ra, int ga, int ba, palette *pal)
{
  palette npal;
  memset(npal.addr(),0,256);

  for (int i=0; i<256; i++)
  {
    npal.set(i,(int)rs,(int)gs,(int)bs);
    rs+=ra; if (rs>255) rs=255; if (rs<0) rs=0;
    gs+=ga; if (gs>255) gs=255; if (gs<0) gs=0;
    bs+=ba; if (bs>255) bs=255; if (bs<0) bs=0;
  }
  filter f(pal,&npal);
  filter f2(&npal,pal);

  for (i=0; i<256; i++,tint++)
    *tint=f2.get_mapping(f.get_mapping(i));
}


void calc_light_table(palette *pal)
{
  white_light=(unsigned char *)malloc(256*64);
  green_light=(unsigned char *)malloc(256*64);
  for (int i=0; i<TTINTS; i++)
      tints[i]=(uchar *)malloc(256);

  jFILE fp("light.tbl","rb");
  int recalc=0;
  if (fp.open_failure()) recalc=1;
  else
  {
    if (fp.read_uint16()!=calc_crc((unsigned char *)pal->addr(),768))
      recalc=1;
    else
    {
      fp.read(white_light,256*64);
      fp.read(green_light,256*64);
      for (i=0; i<TTINTS; i++)
        fp.read(tints[i],256);
    }
  }

  if (recalc)
  {
    fprintf(stderr,"Palette has changed, recalculating light table...\n");
    fprintf(stderr,"white : ");
    for (int color=0; color<256; color++)
    {
      unsigned char r,g,b;
      pal->get(color,r,g,b);
      if (color%16==0)
        fprintf(stderr,"%d ",color);
      for (int intensity=63; intensity>=0; intensity--)
      {
    if (r>0 || g>0 || b>0)
          white_light[intensity*256+color]=pal->find_closest(r,g,b);
    else
          white_light[intensity*256+color]=0;
    if (r) r--;  if (g) g--;  if (b) b--;
      }
    }

    fprintf(stderr,"green : ");
    for (color=0; color<256; color++)
    {
      unsigned char r,g,b;
      pal->get(color,r,g,b);
      r=r*3/5; b=b*3/5; g+=7; if (g>255) g=255;
      if (color%16==0)
        fprintf(stderr,"%d ",color);
      for (int intensity=63; intensity>=0; intensity--)
      {
    if (r>0 || g>0 || b>0)
          green_light[intensity*256+color]=pal->find_closest(r,g,b);
    else
          green_light[intensity*256+color]=0;
    if (r) r--;
    if ((intensity&1)==1)
      if (g) g--;
    if (b) b--;
      }
    }

    dprintf("\ncalculating tints : \n");
    uchar t[TTINTS*6]={ 0,0,0,0,0,0, // normal
                   0,0,0,1,0,0,     // red
           0,0,0,1,1,0,     // yellow
           0,0,0,1,0,1,     // purple
           0,0,0,1,1,1,     // gray
           0,0,0,0,1,0,     // green
           0,0,0,0,0,1,     // blue
           0,0,0,0,1,1,     // cyan






           0,0,0,0,0,0   // reverse green  (night vision effect)
         } ;
    uchar *ti=t+6;
    uchar *c;
    for (i=0,c=tints[0]; i<256; i++,c++) *c=i;  // make the normal tint (maps everthing to itself)
    for (i=0,c=tints[TTINTS-1]; i<256; i++,c++)  // reverse green
    {
      int r=pal->red(i)/2,g=255-pal->green(i)-30,b=pal->blue(i)*3/5+50;
      if (g<0) g=0;
      if (b>255) b=0;
      *c=pal->find_closest(r,g,b);
    }

    // make the colored tints
    for (i=1; i<TTINTS-1; i++)
    {
      calc_tint(tints[i],ti[0],ti[1],ti[2],ti[3],ti[4],ti[5],pal);
      ti+=6;
    }



    jFILE f("light.tbl","wb");
    f.write_uint16(calc_crc((unsigned char *)pal->addr(),768));
    f.write(white_light,256*64);
    f.write(green_light,256*64);
    for (int i=0; i<TTINTS; i++)
      f.write(tints[i],256);
  }
}


light_patch *light_patch::copy(light_patch *Next)
{
  light_patch *p=new light_patch(x1,y1,x2,y2,Next);
  p->total=total;
  if (total)
  {
    p->lights=(light_source **)malloc(total*sizeof(light_source *));
    memcpy(p->lights,lights,total*(sizeof(light_source *)));
  }
  else
    p->lights=NULL;
  return p;
}

#define MAX_LP 10


void add_light(light_patch *&first, long x1, long y1, long x2, long y2,
                light_source *who)
{
  light_patch *last=NULL;
  for (light_patch *p=first; p; p=p->next)
  {
    // first see if light patch we are adding is enclosed entirely by another patch
    if (x1>=p->x1 && y1>=p->y1 && x2<=p->x2 && y2<=p->y2)
    {
      if (p->total==MAX_LP) return ;

      if (x1>p->x1)
      {
    first=p->copy(first);
    first->x2=x1-1;
      }
      if (x2<p->x2)
      {
    first=p->copy(first);
    first->x1=x2+1;
      }
      if (y1>p->y1)
      {
    first=p->copy(first);
    first->x1=x1;
    first->x2=x2;
    first->y2=y1-1;
      }
      if (y2<p->y2)
      {
    first=p->copy(first);
    first->x1=x1;
    first->x2=x2;
    first->y1=y2+1;
      }
      p->x1=x1; p->y1=y1; p->x2=x2; p->y2=y2;
      p->total++;
      p->lights=(light_source **)realloc(p->lights,sizeof(light_source *)*p->total);
      p->lights[p->total-1]=who;
      return ;
    }

    // see if the patch completly covers another patch.
    if (x1<=p->x1 && y1<=p->y1 && x2>=p->x2 && y2>=p->y2)
    {
      if (x1<p->x1)
        add_light(first,x1,y1,p->x1-1,y2,who);
      if (x2>p->x2)
        add_light(first,p->x2+1,y1,x2,y2,who);
      if (y1<p->y1)
        add_light(first,p->x1,y1,p->x2,p->y1-1,who);
      if (y2>p->y2)
        add_light(first,p->x1,p->y2+1,p->x2,y2,who);
      if (p->total==MAX_LP)  return ;
      p->total++;
      p->lights=(light_source **)realloc(p->lights,sizeof(light_source *)*p->total);
      p->lights[p->total-1]=who;
      return ;
    }

    // see if we intersect another rect
    if (!(x2<p->x1 || y2<p->y1 || x1>p->x2 || y1>p->y2))
    {
      int ax1,ay1,ax2,ay2;
      if (x1<p->x1)
      {
        add_light(first,x1,max(y1,p->y1),p->x1-1,min(y2,p->y2),who);
    ax1=p->x1;
      } else
    ax1=x1;

      if (x2>p->x2)
      {
        add_light(first,p->x2+1,max(y1,p->y1),x2,min(y2,p->y2),who);
    ax2=p->x2;
      }
      else
    ax2=x2;

      if (y1<p->y1)
      {
        add_light(first,x1,y1,x2,p->y1-1,who);
    ay1=p->y1;
      } else
    ay1=y1;

      if (y2>p->y2)
      {
        add_light(first,x1,p->y2+1,x2,y2,who);
    ay2=p->y2;
      } else
    ay2=y2;


      add_light(first,ax1,ay1,ax2,ay2,who);

      return ;
    }
  }
}

light_patch *find_patch(int screenx, int screeny, light_patch *list)
{
  for (; list; list=list->next)
  {
    if (screenx>=list->x1 && screenx<=list->x2 && screeny>=list->y1 && screeny<=list->y2)
      return list;
  }
  return NULL;
}

int calc_light_value(long x, long y, light_patch *which)
{
  int lv=0;
  int t=which->total;
  for (register int i=t-1; i>=0; i--)
  {
    light_source *fn=which->lights[i];
    if (fn->type==9)
    {
      lv=fn->inner_radius;
      i=0;
    }
    else
    {
      long dx=abs(fn->x-x)<<fn->xshift;
      long dy=abs(fn->y-y)<<fn->yshift;
      long  r2;
      if (dx<dy)
        r2=dx+dy-(dx>>1);
      else r2=dx+dy-(dy>>1);

      if (r2>=fn->inner_radius)
      {
    if (r2<fn->outer_radius)
    {
      lv+=((fn->outer_radius-r2)*fn->mul_div)>>16;
    }
      } else lv=63;
    }
  }
  if (lv>63) return 63;
  else
    return lv;
}


void reduce_patches(light_patch *f)   // find constant valued patches
{

}

light_patch *make_patch_list(int cx1,int cy1,int cx2,int cy2, long screenx, long screeny)
{
  light_patch *first=new light_patch(cx1,cy1,cx2,cy2,NULL);

  for (light_source *f=first_light_source; f; f=f->next)   // determine which lights will have effect
  {
    long x1=f->x1-screenx,y1=f->y1-screeny,x2=f->x2-screenx,y2=f->y2-screeny;
    if (x1<cx1) x1=cx1;
    if (y1<cy1) y1=cy1;
    if (x2>cx2) x2=cx2;
    if (y2>cy2) y2=cy2;

    if (x1<=x2 && y1<=y2)
      add_light(first,x1,y1,x2,y2,f);
  }
  reduce_patches(first);

  return first;
}

void delete_patch_list(light_patch *first)
{
  while (first)
  {
    light_patch *p=first;
    first=first->next;
    delete p;
  }
}


inline long MAP_PUT(long pad, long screen_addr, long remap, long w)
{ while (w--)
  {
    *((uchar *)(screen_addr))=*((uchar *)remap+*((uchar *)screen_addr));
    screen_addr++;
  }
  return screen_addr;
}

inline long MAP_PUT2(long dest_addr, long screen_addr, long remap, long w)
{ while (w--)
  {
    *((uchar *)(dest_addr))=*((uchar *)remap+*((uchar *)screen_addr));
    screen_addr++;
    dest_addr++;
  }
  return dest_addr;
}

inline long calc_lv(light_patch *lp, long sx, long sy)
{
  light_source **lon_p=lp->lights;
  long lv=min_light_level;
  int light_on;
  for (light_on=lp->total-1; light_on>=0; light_on--)
  {
    light_source *fn=*lon_p;
    long *dt=&(*lon_p)->type;
    if (*dt==9)
    {
      lv=fn->inner_radius;
      light_on=0;
    } else
    {
      dt++;
      int dx=abs(*dt-sx); dt++;
      dx<<=*dt;  dt++;

      int dy=abs(*dt-sy); dt++;
      dy<<=*dt;  dt++;

      int r2;
      if (dx<dy)
      r2=dx+dy-(dx>>1);
      else r2=dx+dy-(dy>>1);

      if (r2<*dt)
      {
    int v=*dt-r2; dt++;
    lv+=v*(*dt)>>16;
      }
    }
    lon_p++;
  }
  if (lv>63) return 63;
  else return lv;

}


void light_screen(image *sc, long screenx, long screeny, uchar *light_lookup)
{
  int lx_run,ly_run;
  switch (light_detail)
  {
    case HIGH_DETAIL :
    { lx_run=2; ly_run=1; } break;
    case MEDIUM_DETAIL :
    { lx_run=3; ly_run=2; } break;
    case LOW_DETAIL :
    { lx_run=4; ly_run=3; } break;
    case POOR_DETAIL :                   // low detail lighting alread taken care of.
    return ;
  }

  int cx1, cy1, cx2, cy2;
  sc->GetClip(cx1, cy1, cx2, cy2);
//  sc->AddDirty(cx1, cy1, cx2, cy2);
  uint8_t *mint = light_lookup + min_light_level * 256;
  screenx-=cx1;
  screeny-=cy1;


  light_patch *first = make_patch_list(cx1, cy1, cx2 - 1, cy2 - 1, screenx, screeny);


  int ytry=(1<<ly_run),xtry=(1<<lx_run);
  int calcx_mask=(0xefffffff-(xtry-1));
  int calcy_mask=(0xefffffff-(ytry-1));
  int scr_w=screen->width();

  for (light_patch *lp=first; lp; lp=lp->next)
  {
    register int x2=lp->x2;
    if (lp->total==0)  // do dark patches really fast.
    {
      unsigned char *sl=screen->scan_line(lp->y1)+lp->x1,*dest;
      dest=sl;

      int y2=lp->y2;
      for (int y=lp->y1; y<=y2; y++,sl+=scr_w,dest+=scr_w)
      {
    int w=lp->x2-lp->x1+1;
        MAP_PUT2((long)dest,(long)sl,(long)mint,w);
      }
    }
    else
    {
      int todoy;
      int y2=lp->y2;

      int xmask=(xtry-1);
      int ymask=(ytry-1);

      uchar *yaddr=screen->scan_line(lp->y1)+lp->x1,*dyaddr;
      dyaddr=yaddr;

      for (int y=lp->y1; y<=y2; )
      {
    long sy=(screeny+y);
    if (y+ytry>lp->y2)        // see how many y lines to do for this block
      todoy=lp->y2-y+1;
    else todoy=ytry;
    int maxy=ytry-(sy&ymask);
    if (todoy>maxy)
      todoy=maxy;

    uchar *xaddr=yaddr,*dxaddr=dyaddr;
    for (register int x=lp->x1; x<=x2; )
    {
      long lv=min_light_level;
      unsigned char *ct;
      light_source *f;
      int todox;
      if (x+xtry>lp->x2)
        todox=lp->x2-x+1;
      else todox=xtry;
      int sx=(screenx+x);
      int max=xtry-(sx&xmask);
      if (todox>max)
        todox=max;


      ct=light_lookup+calc_lv(lp,sx,sy)*256;
      switch (todox)
      {
        case 8 :
        {
          x+=8;
          switch (todoy)
          {
        case 4 :
        {
          uchar *addr=xaddr,*daddr=dxaddr;
          dxaddr=(uchar *)MAP_PUT2((long)daddr,(long)addr,(long)ct,8);
          xaddr+=8;

          addr+=scr_w; daddr+=scr_w;
          MAP_PUT2((long)daddr,(long)addr,(long)ct,8);   addr+=scr_w; daddr+=scr_w;
          MAP_PUT2((long)daddr,(long)addr,(long)ct,8);   addr+=scr_w; daddr+=scr_w;
          MAP_PUT2((long)daddr,(long)addr,(long)ct,8);
        } break;
        case 3 :
        {
          uchar *addr=xaddr,*daddr=dxaddr;
          dxaddr=(uchar *)MAP_PUT2((long)daddr,(long)addr,(long)ct,8);
          xaddr+=8;

          addr+=scr_w; daddr+=scr_w;
          MAP_PUT2((long)daddr,(long)addr,(long)ct,8);   addr+=scr_w; daddr+=scr_w;
          MAP_PUT2((long)daddr,(long)addr,(long)ct,8);


        } break;
        case 2 :
        {
          uchar *addr=xaddr,*daddr=dxaddr;
          dxaddr=(uchar *)MAP_PUT2((long)daddr,(long)addr,(long)ct,8);
          xaddr+=8;

          addr+=scr_w; daddr+=scr_w;
          MAP_PUT2((long)daddr,(long)addr,(long)ct,8);

        } break;
        case 1 :
        { dxaddr=(uchar *)MAP_PUT((long)dxaddr,(long)xaddr,(long)ct,8);
          xaddr+=8;
        } break;
          }
        } break;
/*        case 4 :
        {
          x+=4;
          switch (todoy)
          {
        case 4 :
        {
          uchar *addr=xaddr;
          xaddr=(uchar *)MAP_PUT(0,(long)addr,(long)ct,4); addr+=scr_w; daddr+=scr_w;
          MAP_PUT(0,(long)addr,(long)ct,4);                addr+=scr_w; daddr+=scr_w;
          MAP_PUT(0,(long)addr,(long)ct,4);                addr+=scr_w; daddr+=scr_w;
          MAP_PUT(0,(long)addr,(long)ct,4);
        } break;
        case 3 :
        {
          uchar *addr=xaddr;
          xaddr=(uchar *)MAP_PUT(0,(long)addr,(long)ct,4); addr+=scr_w;  daddr+=scr_w;
          MAP_PUT(0,(long)addr,(long)ct,4);                addr+=scr_w;  daddr+=scr_w;
          MAP_PUT(0,(long)addr,(long)ct,4);
        } break;
        case 2 :
        {
          uchar *addr=xaddr;
          xaddr=(uchar *)MAP_PUT(0,(long)addr,(long)ct,4); addr+=scr_w; daddr+=scr_w;
          MAP_PUT(0,(long)addr,(long)ct,4);
        } break;
        case 1 : xaddr=(uchar *)MAP_PUT(0,(long)xaddr,(long)ct,4); break;
          }
        } break; */
        default :
        {
          x+=todox;
          if (todoy==1)
          {
            dxaddr=(uchar *)MAP_PUT2((long)dxaddr,(long)xaddr,(long)ct,todox);
        xaddr+=todox;
          }
          else
          {
        uchar *addr=xaddr,*daddr=dxaddr;
        dxaddr=(uchar *)MAP_PUT2((long)dxaddr,(long)xaddr,(long)ct,todox);
        xaddr+=todox;

        addr+=scr_w;
        daddr+=scr_w;

        int cc=todoy-1;
        while (cc--)
        {
          MAP_PUT2((long)daddr,(long)addr,(long)ct,todox);
          addr+=scr_w;
          daddr+=scr_w;
        }
          }
        }
      }
    }

    int yadd=scr_w*todoy;
    yaddr+=yadd;
    dyaddr+=yadd;

    y+=todoy;
      }
    }
  }
  while (first)
  {
    light_patch *p=first;
    first=first->next;
    delete p;
  }
}




void add_light_spec(spec_directory *sd, char *level_name)
{
  long size=4+4;  // number of lights and minimum light levels
  for (light_source *f=first_light_source; f; f=f->next)
    size+=6*4+1;
  sd->add(new spec_entry(SPEC_LIGHT_LIST,"lights",NULL,size,0));
}

void write_lights(jFILE *fp)
{
  int t=0;
  for (light_source *f=first_light_source; f; f=f->next) t++;
  fp->write_uint32(t);
  fp->write_uint32(min_light_level);
  for (f=first_light_source; f; f=f->next)
  {
    fp->write_uint32(f->x);
    fp->write_uint32(f->y);
    fp->write_uint32(f->xshift);
    fp->write_uint32(f->yshift);
    fp->write_uint32(f->inner_radius);
    fp->write_uint32(f->outer_radius);
    fp->write_uint8(f->type);
  }
}


int send_lights(net_descriptor *os)
{
  packet pk;
  int t=0;
  for (light_source *f=first_light_source; f; f=f->next) t++;
  pk.write_uint32(t);
  pk.write_uint16(min_light_level);
  if (!os->send(pk)) return 0;
  for (f=first_light_source; f; f=f->next)
  {
    pk.reset();
    pk.write_uint32(f->x);
    pk.write_uint32(f->y);
    pk.write_uint32(f->xshift);
    pk.write_uint32(f->yshift);
    pk.write_uint32(f->inner_radius);
    pk.write_uint32(f->outer_radius);
    pk.write_uint32(f->type);
    if (!os->send(pk)) return 0;
  }
  return 1;
}


void read_lights(spec_directory *sd, jFILE *fp, char const *level_name)
{
  delete_all_lights();
  spec_entry *se=sd->find("lights");
  if (se)
  {
    fp->seek(se->offset,SEEK_SET);
    long t=fp->read_uint32();
    min_light_level=fp->read_uint32();
    light_source *last;
    while (t)
    {
      t--;
      long x=fp->read_uint32();
      long y=fp->read_uint32();
      long xshift=fp->read_uint32();
      long yshift=fp->read_uint32();
      long ir=fp->read_uint32();
      long ora=fp->read_uint32();
      long ty=fp->read_uint8();

      light_source *p=new light_source(ty,x,y,ir,ora,xshift,yshift,NULL);

      if (first_light_source)
        last->next=p;
      else first_light_source=p;
      last=p;
    }
  }
}


int receive_lights(net_descriptor *os)
{
  packet pk;
  if (!os->get(pk)) { printf("net error : light total\n"); return 0; }
  long t;
  if (pk.read((uchar *)&t,4)!=4)
  { printf("net error : (t) packet incomplete\n"); return 0; }
  t=lltl(t);

  if (pk.read((uchar *)&min_light_level,2)!=2)
  { printf("net error : (minl) packet incomplete\n"); return 0; }
  min_light_level=lstl(min_light_level);

  light_source *last;
  printf("Reading %d lights\n",t);
  while (t)
  {
    if (!os->get(pk)) { printf("net error : read light\n"); return 0; }
    t--;
    long dt[7];
    if (pk.read((uchar *)dt,7*4)!=7*4) return 0;


    light_source *p=new light_source(lltl(dt[6]),
                       lltl(dt[0]),lltl(dt[1]),
                       lltl(dt[4]),lltl(dt[5]),
                       lltl(dt[2]),lltl(dt[3]),NULL);

    if (first_light_source)
      last->next=p;
    else first_light_source=p;
      last=p;
  }
  return 1;
}



