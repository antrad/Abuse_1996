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

#include "volumewindow.h" // class VolumeWindow
#include "property.h"     // class property_manager
#include "gui.h"          // ico_button

VolumeWindow::VolumeWindow() : Jwindow("Volume")
{
    char const *ff = "art/frame.spe";
    u_u = cache.reg(ff, "u_u", SPEC_IMAGE, 1),
    u_d = cache.reg(ff, "u_u", SPEC_IMAGE, 1),
    u_ua = cache.reg(ff, "u_ua", SPEC_IMAGE, 1),
    u_da = cache.reg(ff, "u_da", SPEC_IMAGE, 1),
    d_u = cache.reg(ff, "d_u", SPEC_IMAGE, 1),
    d_d = cache.reg(ff, "d_u", SPEC_IMAGE, 1),
    d_ua = cache.reg(ff, "d_ua", SPEC_IMAGE, 1),
    d_da = cache.reg(ff, "d_da", SPEC_IMAGE, 1),
    slider = cache.reg(ff, "volume_slide", SPEC_IMAGE, 1);
    m_pos.x = prop->getd("volume_x", xres / 2 - 20);
    m_pos.y = prop->getd("volume_y", yres / 2 - 50);
    inm->add(new ico_button(10, 27, ID_SFX_DOWN, d_u, d_d, d_ua, d_da,
                  new ico_button(21, 27, ID_SFX_UP, u_u, u_d, u_ua, u_da,
                      new info_field(15, 42, 0, symbol_str("SFXv"),
                          new ico_button(10, 72, ID_MUSIC_DOWN, d_u, d_d, d_ua, d_da,
                              new ico_button(21, 72, ID_MUSIC_UP, u_u, u_d, u_ua, u_da,
                                  new info_field(10, 86, 0, symbol_str("MUSICv"), NULL)))))));

    //reconfigure();
    bg = cache.reg(ff, "vcontrol", SPEC_IMAGE, 1);
    m_size = cache.img(bg)->Size();
    m_surf = new image(m_size, NULL, 2);
    redraw();
}

void VolumeWindow::redraw()
{
    m_surf->PutImage(cache.img(bg), ivec2(0, 0));
    draw_music_vol();
    draw_sfx_vol();
    inm->redraw();
}

void VolumeWindow::draw_vol(int x1, int y1, int x2, int y2, int t,
                            int max, int c1, int c2)
{
    int dx = x1 + t * (x2 - x1) / max;
    if(t != 0)
    {
        m_surf->PutImage(cache.img(slider), ivec2(x1, y1));
//      m_surf->bar(x1,y1,dx,y2,c1);
    }
    else
        dx--;

    if(dx < x2)
        m_surf->Bar(ivec2(dx + 1, y1), ivec2(x2, y2), c2);
}

void VolumeWindow::draw_sfx_vol()
{
    draw_vol(6, 16, 34, 22, sfx_volume, 127,
             pal->find_closest(200, 75, 19), pal->find_closest(40, 0, 0));
}

void VolumeWindow::draw_music_vol()
{
    draw_vol(6, 61, 34, 67, music_volume, 127,
             pal->find_closest(255, 0, 0), pal->find_closest(40, 0, 0));
}

