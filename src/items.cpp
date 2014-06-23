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

#include "items.h"
#include "lisp.h"
#include "dev.h"


extern palette *pal;

boundary::boundary(bFILE *fp, char const *er_name) : point_list(fp)
{
  int x1,y1,x2,y2,checkx,checky,i;
  if (tot)
  {
    if (!(data[0]==data[(tot-1)*2] &&
      data[1]==data[tot*2-1]))
    {
      printf("%s : Endpoints of foretile do not match start points\n",er_name);
      exit(0);
    }

    inside=(uint8_t *)malloc(tot);
  }

  uint8_t *point_on;

  for (i=0,point_on=data; i<tot-1; i++)
  {
    x1=*(point_on++);
    y1=*(point_on++);
    x2=point_on[0];
    y2=point_on[1];

    checkx=(x1+x2)/2;
    checky=(y1+y2)/2;

    int j,xp1,yp1,xp2,yp2,maxx,maxy,minx,miny;
    uint8_t *point2,segs_left=0,segs_right=0,segs_down=0;
    int skip_next=0;
    int check_left=0,check_right=0,check_down=0;


    if (y1==y2) check_down=1;
    else if (x1==x2) check_left=1;
    else
    {
      check_down=1;
      if (x1<x2)
        if (y1<y2) check_left=1;
        else check_right=1;
      else if (y1<y2) check_right=1;
      else check_left=1;
    }

    maxx=Max(x1,x2);
    maxy=Max(y1,y2);
    minx=Min(x1,x2);
    miny=Min(y1,y2);

    if (skip_next)
      skip_next=0;
    else
    {
      for (j=0,point2=data; j<tot-1; j++,point2+=2)
      {
    if (skip_next)
      skip_next=0;
    else
    {
      if (j!=i)           // make sure we are not looking at ourself
      {
        xp1=point2[0];
        yp1=point2[1];
        xp2=point2[2];
        yp2=point2[3];

        if ((checkx>=xp1 && checkx<=xp2) || (checkx>=xp2 &&  checkx<=xp1))
            {
            if (check_down && (yp1>miny && yp2>miny))
        segs_down++;
          if (checkx==xp2) skip_next=1;
        } else if ((checky>=yp1 && checky<=yp2) || (checky>=yp2 &&  checky<=yp1))
        {
          if (check_left && xp1<maxx && xp2<maxx)
        segs_left++;
          if (check_right && xp1>minx && xp2>minx)
            segs_right++;
          if (checky==yp2) skip_next=1;
        }
      }
    }
      }
    }
    if (!check_down) segs_down=1;
    if (!check_right) segs_right=1;
    if (!check_left) segs_left=1;

    inside[i]=!(((segs_left&1)&&(segs_right&1)&&(segs_down&1)));
  }
}

boundary::boundary(boundary *p) : point_list(p->tot,p->data)
{
  int x1,y1,x2,y2,checkx,checky,i;
  uint8_t *point_on;
  if (tot)
  {
    inside=(uint8_t *)malloc(tot);
  } else inside=NULL;
  for (i=0,point_on=data; i<tot-1; i++)
  {
    x1=*(point_on++);
    y1=*(point_on++);
    x2=point_on[0];
    y2=point_on[1];

    checkx=(x1+x2)/2;
    checky=(y1+y2)/2;

    int j,xp1,yp1,xp2,yp2,maxx,maxy,minx,miny;
    uint8_t *point2,segs_left=0,segs_right=0,segs_down=0;
    int skip_next=0;
    int check_left=0,check_right=0,check_down=0;


    if (y1==y2) check_down=1;
    else if (x1==x2) check_right=1;
    else
    {
      check_down=1;
      if (x1<x2)
        if (y1<y2) check_left=1;
        else check_right=1;
      else if (y1<y2) check_right=1;
      else check_left=1;
    }



    maxx=Max(x1,x2);
    maxy=Max(y1,y2);
    minx=Min(x1,x2);
    miny=Min(y1,y2);

    if (skip_next)
      skip_next=0;
    else
    {
      for (j=0,point2=data; j<tot-1; j++,point2+=2)
      {
    if (skip_next)
      skip_next=0;
    else
    {
      if (j!=i)           // make sure we are not looking at ourself
      {
        xp1=point2[0];
        yp1=point2[1];
        xp2=point2[2];
        yp2=point2[3];

        if ((checkx>=xp1 && checkx<=xp2) || (checkx>=xp2 &&  checkx<=xp1))
            {
            if (check_down && (yp1>miny && yp2>miny))
        segs_down++;
          if (checkx==xp2) skip_next=1;
        } else if ((checky>=yp1 && checky<=yp2) || (checky>=yp2 &&  checky<=yp1))
        {
          if (check_left && xp1<maxx && xp2<maxx)
        segs_left++;
          if (check_right && xp1>minx && xp2>minx)
            segs_right++;
          if (checky==yp2) skip_next=1;
        }
      }
    }
      }
    }
    if (!check_down) segs_down=1;
    if (!check_right) segs_right=1;
    if (!check_left) segs_left=1;

    inside[i]=!(((segs_left&1)&&(segs_right&1)&&(segs_down&1)));
  }
}

backtile::backtile(bFILE *fp)
{
  im=load_image(fp);
  next=fp->read_uint16();
}

backtile::backtile(spec_entry *e, bFILE *fp)
{
  im=load_image(e,fp);
  next=fp->read_uint16();
}

foretile::foretile(bFILE *fp)
{
    uint8_t *sl;
    image *img = load_image(fp);

    // create the micro image of the fore tile by averaging the color values
    // in 2×2 space and storing the closest match

    //uint8_t *buffer=(uint8_t *)&micro_image;
    int x, y, w = img->Size().x, h = img->Size().y, l;
    int r[AUTOTILE_WIDTH * AUTOTILE_HEIGHT],
        g[AUTOTILE_WIDTH * AUTOTILE_HEIGHT],
        b[AUTOTILE_WIDTH * AUTOTILE_HEIGHT],
        t[AUTOTILE_WIDTH * AUTOTILE_HEIGHT];
    memset(t, 0, AUTOTILE_WIDTH * AUTOTILE_HEIGHT * sizeof(int));
    memset(r, 0, AUTOTILE_WIDTH * AUTOTILE_HEIGHT * sizeof(int));
    memset(g, 0, AUTOTILE_WIDTH * AUTOTILE_HEIGHT * sizeof(int));
    memset(b, 0, AUTOTILE_WIDTH * AUTOTILE_HEIGHT * sizeof(int));

  if (!pal)
  {
    lbreak("Palette has no been defined\nuse load_palette before load_tiles");
    exit(0);
  }

  if (!color_table)
  {
    lbreak("color filter has no been defined\nuse load_color_filter before load_tiles");
    exit(0);
  }

  for (y=0; y<h; y++)
  {
    sl=img->scan_line(y);
    for (x=0; x<w; x++,sl++)
    {
      l=(y*AUTOTILE_HEIGHT/h)*AUTOTILE_WIDTH +  x*AUTOTILE_WIDTH/w;
      r[l]+=pal->red(*sl);
      g[l]+=pal->green(*sl);
      b[l]+=pal->blue(*sl);
      t[l]++;
    }
  }
  micro_image = new image(ivec2(AUTOTILE_WIDTH, AUTOTILE_HEIGHT));

  for (l=0; l<AUTOTILE_WIDTH*AUTOTILE_HEIGHT; l++)
    micro_image->PutPixel(ivec2(l % AUTOTILE_WIDTH, l / AUTOTILE_WIDTH),
       color_table->Lookup((r[l]/(t[l]*4/5))>>3,
                 (g[l]/(t[l]*4/5))>>3,
                 (b[l]/(t[l]*4/5))>>3));


  im=new TransImage(img,"foretile");
  delete img;

  next=fp->read_uint16();
  fp->read(&damage,1);


  points=new boundary(fp,"foretile boundry");


}

size_t figure::MemUsage()
{
    return forward->DiskUsage() + backward->DiskUsage() + hit->size()
            + f_damage->size() + b_damage->size() + sizeof(figure);
}


figure::figure(bFILE *fp, int type)
{
  image *im=load_image(fp);
  forward=new TransImage(im,"figure data");
  im->FlipX();
  backward=new TransImage(im,"figure backward data");
  delete im;

  fp->read(&hit_damage,1);

  fp->read(&xcfg,1);
  xcfg=xcfg*scale_mult/scale_div;

  if (type==SPEC_CHARACTER)
  {
    point_list p(fp);
    advance=0;
  } else advance=fp->read_uint8();

  f_damage=new boundary(fp,"fig bound");
  b_damage=new boundary(f_damage);
  hit=new point_list(fp);
}


char_tint::char_tint(bFILE *fp)  // se should be a palette entry
{
  palette *p=new palette(fp);
  uint8_t *t=data,*p_addr=(uint8_t *)p->addr();
  for (int i=0; i<256; i++,t++,p_addr+=3)
    *t=pal->find_closest(*p_addr,p_addr[1],p_addr[2]);

  delete p;
}




