/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __COMMON_H__
#define __COMMON_H__

//
// Globally required headers
//
#include <stdint.h>
#include <stdio.h>

//
// Lol Engine
//
#include "lol/matrix.h"
#include "lol/timer.h"
using namespace lol;

//
// Custom utility functions
//
static inline int Min(int a, int b) { return a < b ? a : b; }
static inline int Max(int a, int b) { return a > b ? a : b; }
static inline unsigned int Min(unsigned int a, unsigned int b) { return a < b ? a : b; }
static inline unsigned int Max(unsigned int a, unsigned int b) { return a > b ? a : b; }
static inline long Min(long a, long b) { return a < b ? a : b; }
static inline long Max(long a, long b) { return a > b ? a : b; }
static inline unsigned long Min(unsigned long a, unsigned long b) { return a < b ? a : b; }
static inline unsigned long Max(unsigned long a, unsigned long b) { return a > b ? a : b; }
static inline float Min(float a, float b) { return a < b ? a : b; }
static inline float Max(float a, float b) { return a > b ? a : b; }

//
// Byte swapping
//
static inline int BigEndian()
{
    union { uint32_t const x; uint8_t t[4]; } const u = { 0x01ffff00 };
    return u.t[0];
}

static inline uint16_t lstl(uint16_t x)
{
    if (BigEndian())
        return ((uint16_t)x << 8 ) | ((uint16_t)x >> 8);
    return x;
}

static inline uint32_t lltl(uint32_t x)
{
    if (BigEndian())
        return ((uint32_t)x >> 24) | (((uint32_t)x & 0x00ff0000) >> 8)
             | (((uint32_t)x & 0x0000ff00) << 8) | ((uint32_t)x << 24);
    return x;
}

#define ERROR(x,st) { if (!(x)) \
   { printf("Error on line %d of %s : %s\n", \
     __LINE__,__FILE__,st); exit(1); } }

// These macros should be removed for the non-debugging version
#ifdef NO_CHECK
#   define CONDITION(x,st)
#   define CHECK(x)
#else
#   define CONDITION(x,st) ERROR(x,st)
#   define CHECK(x) CONDITION(x,"Check stop");
#endif

#endif // __COMMON_H__

