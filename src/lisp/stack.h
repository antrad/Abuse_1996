/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __STACK_HPP_
#define __STACK_HPP_

#include <stdio.h>
#include <stdlib.h>

// A fixed-size stack class
template<class T> class GrowStack
{
public:
    GrowStack(int max_size)
    {
        m_max_size = max_size;
        m_size = 0;
        sdata = (T **)malloc(sizeof(T *) * m_max_size);
    }

    ~GrowStack()
    {
        if (m_size != 0)
            fprintf(stderr, "warning: cleaning up nonempty stack\n");
        free(sdata);
    }

    void push(T *data)
    {
        if (m_size >= m_max_size)
        {
            lbreak("error: stack overflow (%d >= %d)\n",
                   (int)m_size, (int)m_max_size);
            exit(1);
        }
        sdata[m_size] = data;
        m_size++;
    }

    T *pop(size_t total)
    {
        if (total > m_size)
        {
            lbreak("error: stack underflow\n");
            exit(1);
        }
        m_size -= total;
        return sdata[m_size];
    }

public:
    T **sdata;
    size_t m_size;

private:
    size_t m_max_size;
};

#endif

