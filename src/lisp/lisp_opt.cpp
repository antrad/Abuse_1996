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

#ifdef NO_LIBS
#include "fakelib.h"
#endif

#include "lisp.h"
#include "lisp_gc.h"

LObject *l_undefined;
LSymbol *true_symbol = NULL, *list_symbol, *string_symbol, *quote_symbol,
     *backquote_symbol, *comma_symbol, *do_symbol, *in_symbol, *aref_symbol,
     *if_symbol, *progn_symbol, *car_symbol, *cdr_symbol;

void *colon_initial_contents, *colon_initial_element,
     *eq_symbol, *zero_symbol, *eq0_symbol, *load_warning;

void *if_1progn,*if_2progn,*if_12progn,*not_symbol;

void *comp_optimize(void *list)
{
  void *return_val=list;
  PtrRef r1(list);
  if (list)
  {
    if (CAR(list)==if_symbol)
    {
      void *eval1=lcar(lcdr(lcdr(list)));
      PtrRef r2(eval1);
      void *eval2=lcar(lcdr(lcdr(lcdr(list))));
      PtrRef r3(eval2);

      void *ret=NULL;
      PtrRef r4(ret);
      if (lcar(list)==eq_symbol && (lcar(lcdr(list))==zero_symbol))  //  simplify (eq 0 x) -> (eq0 x)
      {
    push_onto_list(lcar(lcdr(lcdr(list))),ret);
    push_onto_list(eq0_symbol,ret);
    return_val=comp_optimize(ret);
      } else if (lcar(list)==eq_symbol &&
         (lcar(lcdr(lcdr(list)))==zero_symbol)) //simplify (eq x 0)-> (eq0 x)
      {
    push_onto_list(lcar(lcdr(list)),ret);
    push_onto_list(eq0_symbol,ret);
    return_val=comp_optimize(ret);
      } else if (lcar(lcar(lcdr(list)))==not_symbol)  // simplify (if (not y) x z) -> (if y z x)
      {
    push_onto_list(lcar(lcdr(lcdr(list))),ret);
    push_onto_list(lcar(lcdr(lcdr(lcdr(list)))),ret);
    push_onto_list(lcar(lcdr(lcar(lcdr(list)))),ret);
    push_onto_list(if_symbol,ret);
    return_val=comp_optimize(ret);
      }
      else if (lcar(eval1)==progn_symbol && (eval2==NULL ||
                         item_type(eval2)!=L_CONS_CELL))
      {
    push_onto_list(eval2,ret);
    push_onto_list(lcdr(eval1),ret);
    push_onto_list(lcar(lcdr(list)),ret);
    push_onto_list(if_1progn,ret);
    return_val=comp_optimize(ret);
      } else if (lcar(eval1)==progn_symbol && lcar(eval2)==progn_symbol)
      {
    push_onto_list(lcdr(eval2),ret);
    push_onto_list(lcdr(eval1),ret);
    push_onto_list(lcar(lcdr(list)),ret);
    push_onto_list(if_12progn,ret);
    return_val=comp_optimize(ret);
      } else if (lcar(eval2)==progn_symbol)
      {
    push_onto_list(lcdr(eval2),ret);
    push_onto_list(eval1,ret);
    push_onto_list(lcar(lcdr(list)),ret);
    push_onto_list(if_2progn,ret);
    return_val=comp_optimize(ret);
      }

    }
  }
  return return_val;
}

void Lisp::InitConstants()
{
    // This needs to be defined first
    LSymbol *tmp = LSymbol::FindOrCreate(":UNDEFINED");
    l_undefined = tmp;
    // Collection problems result if we don't do this
    tmp->m_function = NULL;
    tmp->m_value = NULL;

    true_symbol = LSymbol::FindOrCreate("T");

    list_symbol = LSymbol::FindOrCreate("list");
    string_symbol = LSymbol::FindOrCreate("string");
    quote_symbol = LSymbol::FindOrCreate("quote");
    backquote_symbol = LSymbol::FindOrCreate("backquote");
    comma_symbol = LSymbol::FindOrCreate("comma");
    in_symbol = LSymbol::FindOrCreate("in");
    do_symbol = LSymbol::FindOrCreate("do");
    aref_symbol = LSymbol::FindOrCreate("aref");
    colon_initial_contents = LSymbol::FindOrCreate(":initial-contents");
    colon_initial_element = LSymbol::FindOrCreate(":initial-element");

    if_1progn = LSymbol::FindOrCreate("if-1progn");
    if_2progn = LSymbol::FindOrCreate("if-2progn");
    if_12progn = LSymbol::FindOrCreate("if-12progn");
    if_symbol = LSymbol::FindOrCreate("if");
    progn_symbol = LSymbol::FindOrCreate("progn");
    not_symbol = LSymbol::FindOrCreate("not");
    eq_symbol = LSymbol::FindOrCreate("eq");
    zero_symbol = LSymbol::FindOrCreate("0");
    eq0_symbol = LSymbol::FindOrCreate("eq0");
    car_symbol = LSymbol::FindOrCreate("car");
    cdr_symbol = LSymbol::FindOrCreate("cdr");
    load_warning = LSymbol::FindOrCreate("load_warning");
}

