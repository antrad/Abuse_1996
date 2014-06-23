/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef _PALETTE_H_
#define _PALETTE_H_
#include "linked.h"
#include "specs.h"
#define COLOR_BITS 6    // On PC-6, most others -8
#define COLOR_SHIFT (8-COLOR_BITS)
#define MAX_COLOR ((1<<COLOR_BITS)-1)
#define RED3(x)  (unsigned char) ((((int)x&(7<<5))>>5)*(int)255/(int)7)
#define GREEN3(x) (unsigned char) (((x&(7<<2))>>2)*(int)255/(int)7)
#define BLUE2(x) (unsigned char) ((x&3)*(int)255/(int)3)


struct color
{
  unsigned char red,green,blue;
} ;

class palette : public linked_node
{
  color *pal;
  unsigned char *usd;           // bit array
  short ncolors;
  int bg;
public :
  palette(int number_colors=256);
  palette(spec_entry *e, bFILE *fp);
  palette(bFILE *fp);
  void set(int x, unsigned char red, unsigned char green, unsigned char blue);
  void get(int x, unsigned char &red, unsigned char &green, unsigned char &blue);
  uint32_t getquad(int x);
  unsigned int red(int x) { return pal[x].red; }
  unsigned int green(int x) { return pal[x].green; }
  unsigned int blue(int x) { return pal[x].blue; }
  void *addr() { return (void *) pal; }
  void shift(int amount);
  void load();
  void load_nice();
  void fade_to(int total_fades, int fade_on, int dest_r, int dest_g, int dest_b);

  void defaults();
  void set_rgbs();
  void make_black_white();
  void black_white();

  int pal_size() { return ncolors; }
  void set_used(int color_num);
  void set_unused(int color_num);
  int used(int color_num);
  void set_all_used();
  void set_all_unused();
  int find_color(uint8_t r, uint8_t g, uint8_t b);
  int find_closest(uint8_t r, uint8_t g, uint8_t b);
  palette *copy();
  uint8_t brightest(int all=0);
  uint8_t darkest(int all=0, int noblack=1);
  int write(bFILE *fp);
  int size();
  ~palette();
} ;

class quant_node : public linked_node
{
  quant_node *padre;
public :
  unsigned tot;
  quant_node *children[8];
  int is_leaf() { return children[0]==this; }
  void be_childish() { children[0]=this; }
  quant_node *father() { return padre; }
  quant_node(int level, quant_node *dad,
    unsigned char r=0, unsigned char g=0, unsigned char b=0);
  void total(int &tnodes, int &tr, int &tg, int &tb);
//  void prune();
  void set(int r,int g,int b) { red=r; green=g; blue=b; }
  unsigned char red,green,blue;
  ~quant_node();
} ;


class quant_palette
{
  linked_list level[8];
  quant_node *root;
  int nc,mx;
  void prune();
  void re_delete(quant_node *who, int lev);
public :
  quant_palette(int max_colors=256);
  ~quant_palette();
} ;

palette *last_loaded();
#endif
