/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __CLISP_HPP_

int get_lprop_number(void *sybol, int def);  // returns def if symbol undefined or not number type


// variables for the status bar
extern LSymbol *l_chat_input, *l_post_render;

// FIXME: port these to LSymbol
extern void *l_statbar_ammo_x,*l_statbar_ammo_y,
            *l_statbar_ammo_w,*l_statbar_ammo_h,
        *l_statbar_ammo_bg_color,

            *l_statbar_health_x,*l_statbar_health_y,
            *l_statbar_health_w,*l_statbar_health_h,
        *l_statbar_health_bg_color,

        *l_statbar_logo_x,*l_statbar_logo_y,
        *l_object,*l_tile,*l_fire_object,
        *l_player_draw,*l_sneaky_draw,
        *l_draw_fast,*l_player_tints,*l_next_song,
        *l_level_load_start,
        *l_level_load_end,
        *l_cdc_logo,
        *l_keep_backup,
        *l_switch_to_powerful,
        *l_mouse_can_switch,
        *l_ask_save_slot,
        *l_get_local_input,
        *l_player_text_color,
        *l_level_loaded;        // called when a new level is loaded


/******************************** Lisp objects **********************************/
extern LSymbol *l_difficulty, *l_easy, *l_hard, *l_medium, *l_extreme,
    *l_max_hp, *l_max_power,
    *l_empty_cache;

// FIXME: port these to LSymbol
extern void *l_main_menu,
     *l_logo,*l_state_art,*l_default_abilities,*l_abilities,
     *l_default_ai_function,*l_state_sfx, *l_morph,
     *l_song_list,*l_filename,*l_sfx_directory,*l_default_font,
     *l_range,*l_joy_file,*l_death_handler,
     *l_title_screen,*l_console_font,*l_fields,*l_FIRE,*l_fire_object,
     *l_cop_dead_parts,*l_restart_player,*l_help_screens,*l_save_order;


#endif
