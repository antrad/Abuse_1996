/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef _VIEW_HPP_
#define _VIEW_HPP_

#include "light.h"
#include "jwindow.h"


class object_node;
class game_object;
class area_controller;

struct suggest_struct
{
  int32_t cx1,cy1,cx2,cy2,shift_down,shift_right,pan_x,pan_y;
  int32_t new_weapon;
  uint8_t send_view,send_weapon_change;
} ;


class view;


class view
{
  uint8_t keymap[512/8];
  char chat_buf[60];
  public :
  int key_down(int key) { return keymap[key/8]&(1<<(key%8)); }
  void set_key_down(int key, int x) { if (x) keymap[key/8]|=(1<<(key%8)); else keymap[key/8]&=~(1<<(key%8)); }
  void reset_keymap() { memset(keymap,0,sizeof(keymap)); }
  void add_chat_key(int key);

  char name[100];
  struct suggest_struct suggest;
  int32_t cx1,cy1,cx2,cy2,                    // view area to show
      shift_down,shift_right;             // shift of view

  int god;                                // :) if you believe in such things
  int player_number;
  int _tint, _team;

  int draw_solid;                         // -1 if don't draw solid

  int32_t *weapons;                          // [0..total_weapons-1]
  int32_t *last_weapons;                     // last history of above array (for updating statbar)
  int32_t current_weapon;


  game_object *focus;                     // object we are focusing on (player)
  int x_suggestion,                      // input from the player at the current time
      y_suggestion,
      b1_suggestion,
      b2_suggestion,
      b3_suggestion,
      b4_suggestion,
      pointer_x,
      pointer_y,
      freeze_time;


  short ambient;                        // ambient lighting setting, used by draw

  int32_t pan_x,pan_y,no_xleft,no_xright,no_ytop,no_ybottom,
       last_x,last_y,last_last_x,last_last_y,view_percent;

  int32_t last_left,last_right,last_up,last_down,   // how many frames ago were these pressed (<=0)
       last_b1,last_b2,last_b3,last_b4,last_hp,last_ammo,last_type;
  int32_t secrets,kills,tsecrets,tkills;

  view(game_object *Focus, view *Next, int number);
  ~view();

  void draw_character_damage();           // draws the characters 'status' on the viewer

  int32_t x_center();                        // center of attention
  int32_t y_center();
  int32_t xoff();                            // top left and right corner of the screen
  int32_t interpolated_xoff();
  int32_t yoff();
  int32_t interpolated_yoff();
  int drawable();                        // network viewables are not drawable
  int local_player();                    //  just in case I ever need non-viewable local players.

  view *next;                             // next viewable player (singly linked list)
  void get_input();
  int process_input(char cmd, uint8_t *&pk);

  void add_ammo   (int weapon_type, int total);
  int has_weapon  (int weapon_type) { return god || (weapons[weapon_type]!=-1); }
  void give_weapon(int weapontype);
  int weapon_total(int weapon_type);

  void note_upkey();
  void note_downkey();
  int handle_event(event &ev);
  void update_scroll();
  void draw_hp();
  void draw_ammo();
  void draw_logo();
  void resize_view(int32_t Cx1, int32_t Cy1, int32_t Cx2, int32_t Cy2);
  void set_input(int cx, int cy, int b1, int b2, int b3, int b4, int px, int py);
  int view_changed() { return suggest.send_view; }
  int weapon_changed() { return suggest.send_weapon_change; }

  void next_weapon();
  void last_weapon();

  void reset_player();
  int receive_failed() { return focus==NULL; }
  int32_t get_view_var_value(int num);
  int32_t set_view_var_value(int num, int32_t x);
  void configure_for_area(area_controller *a);

  void set_tint(int);
  int get_tint();
  void set_team(int);
  int get_team();
};

extern view *player_list;
void set_local_players(int total);
int total_local_players();
void recalc_local_view_space();

void process_packet_commands(uint8_t *pk, int size);

object_node *make_player_onodes(int player_num=-1);
int total_view_vars();
char const *get_view_var_name(int num);
uint16_t make_sync();

#endif





