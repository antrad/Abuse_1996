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
#include <stdint.h>

void pushback(int32_t x1,int32_t y1,int32_t &x2,int32_t &y2,
             int32_t xp1, int32_t yp1, int32_t xp2, int32_t yp2, int xdir, int ydir, int inside)
{

   // determine if the lines are intersecting before we set back

//   if (yp1==yp2)
//   {
//     if (inside && y2>=yp1)

}


/* int setback_intersect(int32_t x1,int32_t y1,int32_t &x2,int32_t &y2,
             int32_t xp1, int32_t yp1, int32_t xp2, int32_t yp2)
{
  int32_t mx1,my1,b1,mx2,my2,b2,side1,side2,tx2,ty2;
  my1=(y2-y1);
  if (!my1)                  // is the first line strait across?
  {
    side1=yp1-y2;          // if yes, check to see if the other is top/bottom
    side2=yp2-y2;
    mx1=(x2-x1);          // make sure we give this a value
    b1=y2;
  }
  else
  {
    mx1=(x2-x1);           // if the line strait up and down?
    if (!mx1)
    {
      side1=xp1-x1;
      side2=xp2-x2;
    } else
    {
      b1=y1-(my1*x1/mx1);    // calculate the y intercept
      side1=yp1-((my1*xp1)/mx1+b1);
      side2=yp2-((my1*xp2)/mx1+b1);
    }
  }
  if ((side1>=0 && side2<=0) || (side1<=0 && side2>=0))
  {
    my2=(yp2-yp1);
    if (!my2)
    {
      side1=y1-yp2;
      side2=y2-yp2;
      b2=yp1;
      mx2=(xp2-xp1);
    }
    else
    {
      mx2=(xp2-xp1);
      if (!mx2)
      {
    side1=x1-xp2;
    side2=x2-xp2;
      } else
      {
    b2=yp1-(my2*xp1/mx2);
    side1=y1-((my2*x1)/mx2+b2);
    side2=y2-((my2*x2)/mx2+b2);
      }
    }

    // check two wierd cases where the lines are parallel
    if (mx1==mx2 && mx1==0)
      if (y2<yp1 || y1>yp2)
        side1=side2=1;
      else side1=side2=0;

    if (my1==my2 && my2==0)
      if (x2<xp1 || x1>xp2)
        side1=side2=1;
      else side1=side2=0;

    if ((side1>=0 && side2<=0) || (side1<=0 && side2>=0))
    {
      tx2=x2;  ty2=y2;
      int xadd,yadd;
      if (x1>x2) xadd=1; else if (x1<x2) xadd=-1; else xadd=0;
      if (y1>y2) yadd=1; else if (y1<y2) yadd=-1; else yadd=0;

      // now find the point of intersection
      if (mx1==0)  // if the first line is strait up & down
      {
    if (mx2==0)   // if they are both strait up then
    { tx2=x1; ty2=y1; }  // they connect everywhere, don't let it move!
        else
          ty2=my2*x2/mx2+b2+yadd;
      } else if (mx2==0)
      { tx2=xp1+xadd;
        ty2=my1*tx2/mx1+b1;
      }
      else
      {
        if (my1==0)
          if (my2==0)
            { tx2=x1; ty2=y1; }
          else tx2=mx2*(y1-b2)/my2+xadd;
        else if (my2==0)
        {
          ty2=yp1+yadd;
          tx2=mx1*(ty2-b1)/my1;
        }
        else
        {
          if (abs(mx1)>abs(my1))
          {
            int32_t ae_bd=my1*mx2-mx1*my2;
            CONDITION(ae_bd,"line intersect fuck up");
            tx2=(mx1*mx2*(b2-b1))/ae_bd+xadd;
            ty2=my1*tx2/mx1+b1;
          }
          else
          {
            int32_t db_ea=(my2*mx1-mx2*my1);
            CONDITION(db_ea,"line intersect fuck up");
            ty2=(mx1*b1*my2-my1*mx2*b2)/db_ea+yadd;
            tx2=mx1*(ty2-b1)/my1;
          }
        }
      }

      if (abs(tx2-x1)<abs(x2-x1) || abs(ty2-y1)<abs(y2-y1))
      {
        x2=tx2;
        y2=ty2;
        return 1;
      }
    }
  }
  return 0;
} */


int setback_intersect(int32_t x1,int32_t y1,int32_t &x2,int32_t &y2,
              int32_t xp1, int32_t yp1, int32_t xp2, int32_t yp2,
                     int32_t inside)  // which side is inside the polygon? (0 always setback)
{
  // the line equations will be put in the form
  // x(y2-y1)+y(x1-x2)-x1*y2+x2*y1=0
  //     A        B        C

  int32_t a1,b1,c1,a2,b2,c2,r1,r2;

  a1=y2-y1;
  b1=x1-x2;
  c1=-x1*y2+x2*y1;

  if (yp1<yp2 || (yp1==yp2 && xp1>xp2))    // use only increasing functions
  {
    r1=yp1; yp1=yp2; yp2=r1;        // swap endpoints if wrong order
    r1=xp1; xp1=xp2; xp2=r1;
  }

  int32_t xdiff,ydiff;
/*  int32_t xdiff=abs(xp1-xp2),ydiff=yp1-yp2;
  if (xdiff>=ydiff)                              // increment the endpoints
    if (xp2<xp1) { xp2--; xp1++; }
    else { xp2++; xp1--; }

  if (xdiff<=ydiff)
  {
    yp1++;
    yp2--;
  } */


  r1=xp1*a1+yp1*b1+c1;
  r2=xp2*a1+yp2*b1+c1;

  if ((r1^r2)<=0 || r1==0 || r2==0)           // signs must be different to intersect
  {
    a2=yp2-yp1;
    b2=xp1-xp2;
    c2=-xp1*yp2+xp2*yp1;
    r1=x1*a2+y1*b2+c2;
    r2=x2*a2+y2*b2+c2;

    if ((r1^r2)<=0 || r1==0 || r2==0)
    {
      if ( ((xp1<xp2) && ((r2^inside)>0)) ||
           (xp1>=xp2 && ((r2^inside)<0)) ||
       inside==0 || r2==0)
      {
    int32_t ae=a1*b2,bd=b1*a2;
    if (ae!=bd)         // co-linear returns 0
    {
      x2=(b1*c2-b2*c1)/(ae-bd);
      y2=(a1*c2-a2*c1)/(bd-ae);
      xdiff=abs(x2-x1);
      ydiff=abs(y2-y1);
//      if (xdiff<=ydiff)            // push the intersection back one pixel
//      {
        if (y2!=y1)
        {
            if (y2>y1)
            y2--;
          else y2++;
        }
//      }
//      if (xdiff>=ydiff)
//      {
        if (x2!=x1)
        {
          if (x2>x1)
            x2--;
          else x2++;
        }
//      }

      if (inside)        // check to make sure end point is on the
      {                  // right side
        r1=x1*a2+y1*b2+c2;
        r2=x2*a2+y2*b2+c2;
        if ((r2!=0 && ((r1^r2)<0)))
        {
          x2=x1;
          y2=y1;
        }
      }
      return 1;
    }
      }
    }
  }
  return 0;

}

