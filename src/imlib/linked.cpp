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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "linked.h"

linked_list::linked_list()
  : m_first(NULL),
    m_count(0)
{
    ;
}

//
// Clear all the nodes in a linked list and dispose of the
// memory for each one by calling its destructor
//
linked_list::~linked_list()
{
    m_count = 0; // ensure destructors calling unlink() won't mess with us

    if (m_first)
        m_first->Prev()->SetNext(NULL); // set the prev nodes next to NULL
                                        // so we can go until we hit NULL
    while (m_first != NULL)
    {
        linked_node *tmp = m_first->Next();
        delete m_first;
        m_first = tmp;
    }
}

//
// Take a node out of the linked list, but do not dispose of the memory
//
int linked_list::unlink(linked_node *p)
{
    if (!m_count)
        return 0;

    linked_node *q = m_first;

    while (q != p) // find the node in the list
    {
        q = q->Next();
        if (q == m_first)
            return 0; // not in the list!
    }

    q->Prev()->SetNext(q->Next());
    q->Next()->SetPrev(q->Prev());

    if (p == m_first) // if they want to unlink the first node
        m_first = (m_first->Next() == m_first) ? NULL : p->Next();

    m_count--;
    return 1;
}

//
// Add a node to the end of a linked_list
//
void linked_list::add_end(class linked_node *p)
{
    if (m_first)
        p->SetPrev(m_first->Prev());
    else
        m_first = p;
    p->SetNext(m_first);

    if (m_first != p)
        m_first->Prev()->SetNext(p);
    m_first->SetPrev(p);

    m_count++;
}

// Add a node at the front of the list: just add it at the end and set
// the first pointer to it
void linked_list::add_front(class linked_node *p)
{
    add_end(p);
    m_first = p;
}

