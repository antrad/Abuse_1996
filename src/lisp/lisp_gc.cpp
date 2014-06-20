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
#include <string.h>

#include "lisp.h"
#include "lisp_gc.h"

#include "stack.h"

/*  Lisp garbage collection: uses copy/free algorithm
    Places to check:
      symbol
        values
    functions
    names
      stack
*/

// Stack where user programs can push data and have it GCed
GrowStack<void> l_user_stack(150);

// Stack of user pointers
GrowStack<void *> PtrRef::stack(1500);

static size_t reg_ptr_total = 0;
static void ***reg_ptr_list = NULL;

static uint8_t *cstart, *cend, *collected_start, *collected_end;

LArray *LispGC::CollectArray(LArray *x)
{
    size_t s = x->len;
    LArray *a = LArray::Create(s, NULL);
    LObject **src = x->GetData();
    LObject **dst = a->GetData();
    for (size_t i = 0; i < s; i++)
        dst[i] = CollectObject(src[i]);

    return a;
}

LList *LispGC::CollectList(LList *x)
{
    LList *last = NULL, *first = NULL;

    for (; x && item_type(x) == L_CONS_CELL; )
    {
        LList *p = LList::Create();
        LObject *old_car = x->car;
        LObject *old_cdr = x->cdr;
        LObject *old_x = x;
        x = (LList *)CDR(x);
        ((LRedirect *)old_x)->type = L_COLLECTED_OBJECT;
        ((LRedirect *)old_x)->ref = p;

        p->car = CollectObject(old_car);
        p->cdr = CollectObject(old_cdr);

        if (last)
            last->cdr = p;
        else
            first = p;
        last = p;
    }
    if (x)
        last->cdr = CollectObject(x);
    return first; // we already set the collection pointers
}

LObject *LispGC::CollectObject(LObject *x)
{
    LObject *ret = x;

    if ((uint8_t *)x >= cstart && (uint8_t *)x < cend)
    {
        switch (item_type(x))
        {
        case L_BAD_CELL:
            lbreak("error: collecting corrupted cell\n");
            break;
        case L_NUMBER:
            ret = LNumber::Create(((LNumber *)x)->num);
            break;
        case L_SYS_FUNCTION:
            ret = new_lisp_sys_function(((LSysFunction *)x)->min_args,
                                        ((LSysFunction *)x)->max_args,
                                        ((LSysFunction *)x)->fun_number);
            break;
        case L_USER_FUNCTION:
        {
            LUserFunction *fun = (LUserFunction *)x;
            LList *arg = (LList *)CollectObject(fun->arg_list);
            LList *block = (LList *)CollectObject(fun->block_list);
            ret = new_lisp_user_function(arg, block);
            break;
        }
        case L_STRING:
            ret = LString::Create(lstring_value(x));
            break;
        case L_CHARACTER:
            ret = LChar::Create(lcharacter_value(x));
            break;
        case L_C_FUNCTION:
            ret = new_lisp_c_function(((LSysFunction *)x)->min_args,
                                      ((LSysFunction *)x)->max_args,
                                      ((LSysFunction *)x)->fun_number);
            break;
        case L_C_BOOL:
            ret = new_lisp_c_bool(((LSysFunction *)x)->min_args,
                                  ((LSysFunction *)x)->max_args,
                                  ((LSysFunction *)x)->fun_number);
            break;
        case L_L_FUNCTION:
            ret = new_user_lisp_function(((LSysFunction *)x)->min_args,
                                         ((LSysFunction *)x)->max_args,
                                         ((LSysFunction *)x)->fun_number);
            break;
        case L_POINTER:
            ret = LPointer::Create(lpointer_value(x));
            break;
        case L_1D_ARRAY:
            ret = CollectArray((LArray *)x);
            break;
        case L_FIXED_POINT:
            ret = LFixedPoint::Create(lfixed_point_value(x));
            break;
        case L_CONS_CELL:
            ret = CollectList((LList *)x);
            break;
        case L_OBJECT_VAR:
            ret = LObjectVar::Create(((LObjectVar *)x)->index);
            break;
        case L_COLLECTED_OBJECT:
            ret = ((LRedirect *)x)->ref;
            break;
        default:
            lbreak("error: collecting bad object 0x%x\n", item_type(x));
            break;
        }
        ((LRedirect *)x)->type = L_COLLECTED_OBJECT;
        ((LRedirect *)x)->ref = ret;
    }
    else if ((uint8_t *)x < collected_start || (uint8_t *)x >= collected_end)
    {
        // Still need to remap cons_cells lying outside of space, for
        // instance on the stack.
        for (LObject *cell = NULL; x; cell = x, x = CDR(x))
        {
            if (item_type(x) != L_CONS_CELL)
            {
                if (cell)
                    CDR(cell) = CollectObject(CDR(cell));
                break;
            }
            CAR(x) = CollectObject(CAR(x));
        }
    }

    return ret;
}

void LispGC::CollectSymbols(LSymbol *root)
{
    if (!root)
        return;

    root->value = CollectObject(root->value);
    root->function = CollectObject(root->function);
    root->name = (LString *)CollectObject(root->name);
    CollectSymbols(root->left);
    CollectSymbols(root->right);
}

void LispGC::CollectStacks()
{
    void **d = l_user_stack.sdata;
    for (size_t i = 0; i < l_user_stack.m_size; i++, d++)
        *d = CollectObject((LObject *)*d);

    void ***d2 = PtrRef::stack.sdata;
    for (size_t i = 0; i < PtrRef::stack.m_size; i++, d2++)
    {
        void **ptr = *d2;
        *ptr = CollectObject((LObject *)*ptr);
    }

    void ***d3 = reg_ptr_list;
    for (size_t i = 0; i < reg_ptr_total; i++, d3++)
    {
        void **ptr = *d3;
        *ptr = CollectObject((LObject *)*ptr);
    }
}

void LispGC::CollectSpace(int which_space, int grow)
{
    int old_space = current_space;
    cstart = space[which_space];
    cend = free_space[which_space];

    space_size[GC_SPACE] = space_size[which_space];
    if (grow)
    {
        space_size[GC_SPACE] += space_size[which_space] >> 1;
        space_size[GC_SPACE] -= (space_size[GC_SPACE] & 7);
    }
    uint8_t *new_space = (uint8_t *)malloc(space_size[GC_SPACE]);
    current_space = GC_SPACE;
    free_space[GC_SPACE] = space[GC_SPACE] = new_space;

    collected_start = new_space;
    collected_end = new_space + space_size[GC_SPACE];

    CollectSymbols(LSymbol::root);
    CollectStacks();

    // for debuging clear it out
    memset(space[which_space], 0, space_size[which_space]);
    free(space[which_space]);

    space[which_space] = new_space;
    space_size[which_space] = space_size[GC_SPACE];
    free_space[which_space] = new_space
                            + (free_space[GC_SPACE] - space[GC_SPACE]);
    current_space = old_space;
}

