/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

// linked.h  - linked list and linked list node classes
// written June 2, 1992 by Jonathan Clark  (at home)

#ifndef __LINKED_H__
#define __LINKED_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

class linked_node
{
    friend class linked_list;

public:
    linked_node() { m_next = m_prev = NULL; }
    virtual ~linked_node() { ; }

    inline class linked_node *Next() { return m_next; }
    inline class linked_node *Prev() { return m_prev; }
    inline void SetNext(class linked_node *p) { m_next = p; }
    inline void SetPrev(class linked_node *p) { m_prev = p; }

private:
    class linked_node *m_next, *m_prev;
};

// this is the basic class for all linked_list
// its features should be self-explanatory.
// openly use the functions listed after the keyword PUBLIC
// type conversions may be nessary if you derive a class of nodes of your own
// for example shape is an class derived from linked_node. to add a shape to
// linked list I have to say mylist.add_end(myshape_pointer);
// unlink removes a node from the list via pointers but does not deallocate
// it from the heap
// the destructor for linked_list will get dispose of all the nodes as
// well, so if you don't want something deleted then you must unlink
// it from the list before the destructor is called

class linked_list
{
public:
    linked_list();
    ~linked_list();

    void add_front(class linked_node *p);
    void add_end(class linked_node *p);
    int unlink(linked_node *p);

    inline class linked_node *first() { return m_first; }
    inline class linked_node *prev() { return m_first->Prev(); }
    inline size_t Count() { return m_count; }

private:
    linked_node *m_first;
    size_t m_count;
};

#endif

