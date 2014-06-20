/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __LISP_OPT_HPP_
#define __LISP_OPT_HPP_

#include "lisp.h"

extern LObject *l_undefined;
extern LSymbol *true_symbol, *list_symbol, *string_symbol, *quote_symbol,
     *backquote_symbol, *comma_symbol, *do_symbol, *in_symbol, *aref_symbol,
     *if_symbol, *progn_symbol, *car_symbol, *cdr_symbol;

extern void *colon_initial_contents, *colon_initial_element, *load_warning;

#endif
