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

#include "video.h"
#include "image.h"
#include "palette.h"
#include "linked.h"
#include "sprite.h"


void sprite::restore_background()
{ if (x+save->Size().x>=0 && y+save->Size().y>=0 && x<=xres && y<=yres)
      save->put_image(screen,x,y); }

void sprite::get_background()
{ if (x+visual->Size().x>=0 && y+visual->Size().y>=0 && x<=xres && y<=yres)
   screen->put_part(save,0,0,x,y,x+save->Size().x-1,y+save->Size().y-1); }

void sprite::draw()
{ if (x+visual->Size().x>=0 && y+visual->Size().y>=0 && x<=xres && y<=yres)
   visual->put_image(screen,x,y,1); }

sprite::sprite(image *Screen, image *Visual, int X, int Y)
{
  CHECK(Visual && Screen);
  x=X; y=Y; visual=Visual; screen=Screen;
  save = new image(visual->Size());
  get_background();
}

sprite::~sprite()
{
  delete save;
}

void sprite_controller::add_sprite(sprite *sp)
{ sprites.add_end(sp); }

#define loopt(type,controll,first,inside) { controll=(type *)(first); \
  if (first) do { inside controll=(type *)(controll->Next()); } \
  while (controll!=(type *)(first)); }

void sprite_controller::remove_sprites()
{ sprite *sp; loopt(sprite,sp,sprites.first(),sp->restore_background(); ); }

void sprite_controller::put_sprites()
{ sprite *sp; loopt(sprite,sp,sprites.first(),sp->draw(); ); }

void sprite_controller::get_backgrounds()
{ sprite *sp; loopt(sprite,sp,sprites.first(),sp->get_background(); ); }

void sprite::change_visual(image *Visual, int delete_old)
{ if (delete_old)
    delete visual;
  visual=Visual;
  if (save->Size() != Visual->Size())
  {
    delete save;
    save = new image(visual->Size());
  }
  get_background();
}

void sprite_controller::bring_front(sprite *sp)
{
  ERROR(sprites.unlink(sp),"unlink failure");
  sprites.add_end(sp);
}

void sprite_controller::delete_sprite(sprite *sp)
{
  ERROR(sprites.unlink(sp),"unlink failure");
  delete sp;
}

