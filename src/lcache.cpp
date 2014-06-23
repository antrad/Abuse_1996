/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

/*
 * This file contains serialisation methods for the cache system. It
 * is NOT used to load and save games.
 * XXX: this code has not been tested after the LObject refactor.
 */

#if defined HAVE_CONFIG_H
#   include "config.h"
#endif

#include "lisp.h"
#include "specs.h"

size_t block_size(LObject *level)  // return size needed to recreate this block
{
    if (!level) // NULL pointers don't need to be stored
        return 0;

    switch (item_type(level))
    {
    case L_CONS_CELL:
        {
            size_t ret = sizeof(uint8_t) + sizeof(uint32_t);
            void *b = level;
            for (; b && item_type(b) == L_CONS_CELL; b = CDR(b))
                ;
            if (b)
                ret += block_size((LObject *)b);
            for (b = level; b && item_type(b) == L_CONS_CELL; b = CDR(b))
                ret += block_size(CAR(b));
            return ret;
        }
    case L_CHARACTER:
        return sizeof(uint8_t) + sizeof(uint16_t);
    case L_STRING:
        return sizeof(uint8_t) + sizeof(uint32_t)
                               + strlen(lstring_value(level)) + 1;
    case L_NUMBER:
        return sizeof(uint8_t) + sizeof(uint32_t);
    case L_SYMBOL:
        return sizeof(uint8_t) + sizeof(uintptr_t);
    }

    /* Do not serialise other types */
    return 0;
}

void write_level(bFILE *fp, LObject *level)
{
    int type = item_type(level);
    fp->write_uint8(type);

    switch (type)
    {
    case L_CONS_CELL:
        if (!level)
            fp->write_uint32(0);
        else
        {
            size_t count = 0;
            void *b = level;
            for (; b && item_type(b) == L_CONS_CELL; b = CDR(b))
                count++;
            /* If last element is not the empty list, it's a dotted list
             * and we need to save the last object. Write a negative size
             * to reflect that. */
            fp->write_uint32(b ? -(int32_t)count : count);
            if (b)
                write_level(fp, (LObject *)b);

            for (b = level; b && item_type(b) == L_CONS_CELL; b = CDR(b))
                write_level(fp, CAR(b));
        }
        break;
    case L_CHARACTER:
        fp->write_uint16(((LChar *)level)->GetValue());
        break;
    case L_STRING:
        {
            size_t count = strlen(lstring_value(level)) + 1;
            fp->write_uint32(count);
            fp->write(lstring_value(level), count);
        }
        break;
    case L_NUMBER:
        fp->write_uint32(lnumber_value(level));
        break;
    case L_SYMBOL:
        {
            uintptr_t p = (uintptr_t)level;
            for (size_t i = 0; i < sizeof(uintptr_t); i++)
            {
                fp->write_uint8((uint8_t)p);
                p >>= 8;
            }
        }
    }
}

LObject *load_block(bFILE *fp)
{
    int type = fp->read_uint8();

    switch (type)
    {
    case L_CONS_CELL:
        {
            int32_t t = (int32_t)fp->read_uint32();

            if (!t)
                return NULL;

            LList *last = NULL, *first = NULL;
            for (size_t count = abs(t); count--; )
            {
                LList *c = LList::Create();
                if (first)
                    last->m_cdr = c;
                else
                    first = c;
                last = c;
            }
            last->m_cdr = (t < 0) ? (LObject *)load_block(fp) : NULL;

            last = first;
            for (size_t count = abs(t); count--; last = (LList *)last->m_cdr)
                last->m_car = load_block(fp);
            return first;
        }
    case L_CHARACTER:
        return LChar::Create(fp->read_uint16());
    case L_STRING:
        {
            size_t count = fp->read_uint32();
            LString *s = LString::Create(count);
            fp->read(s->GetString(), count);
            return s;
        }
    case L_NUMBER:
        return LNumber::Create(fp->read_uint32());
    case L_SYMBOL:
        {
            uintptr_t ret = 0, mul = 1;
            for (size_t i = 0; i < sizeof(uintptr_t); i++)
            {
                ret |= mul * fp->read_uint8();
                mul *= 8;
            }
            return (LObject *)ret;
        }
    }

    return NULL;
}

