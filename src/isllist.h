/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef ISLLIST_HH
#define ISLLIST_HH

// "inc/isllist.h", line 13.10: 1540-016: (S) protected member "isllist<tcpip_protocol::RequestItem*>::list_node *" cannot be accessed.

template <class T>
class isllist
{
public:
  class list_node
  {
  public:
    list_node * next;
    T data;

    list_node() { }
    list_node(const T& item) { data = item; }
  };

  list_node * list;

  class iterator
  {
  public:
    // pseudo-protected - don't use unless you really have to
    list_node * node;
    iterator(list_node * p) : node(p) { }

    iterator() { }
    iterator(const iterator &p) : node(p.node) { }

    int operator==(const iterator &p) { return (node == p.node); }
    int operator!=(const iterator &p) { return (node != p.node); }

    iterator& operator++() { node = node->next; return *this; }
    iterator next() { return node->next; }

    T& operator*() { return node->data; }
  };

  iterator end()   { return (list_node *)(&list); }
  iterator begin_prev() { return end(); }
  iterator begin() { return list; }

  int empty() { return begin() == end(); }

  iterator insert_next(iterator pos, T& item)
  {
    list_node * p = new list_node(item);
    p->next = pos.node->next;
    pos.node->next = p;

    return p;
  }

  void erase_next(iterator pos)
  {
    list_node * p = pos.node->next;
    pos.node->next = p->next;
    delete p;
  }

  int find_prev(iterator& p, T& item)
  {
    while (p.node->next != end().node)
    {
      if (*(p.next())==item)
        return 1;
      ++p;
    }
    return 0;
  }

  void move_next(const iterator&p, const iterator&q)
  {
    list_node * tmp;

    tmp = p.node->next;
    if (tmp == q.node)
      return;
    p.node->next = tmp->next;
    tmp->next = q.node->next;
    q.node->next = tmp;
  }

  int find(T& item) { iterator p = begin_prev(); return find_prev(p, item); }
  void insert(T& item) {  insert_next( begin_prev(), item); }
  void erase() { erase_next( begin_prev() ); }

  void erase_all()
  {
    while (!empty())
      erase();
  }

  isllist()
  {
    list = (list_node *)&list;
  }

  ~isllist()
  {
    erase_all();
  }
};

#endif

/*
"inc/isllist.h", line 9.8: 1540-051: (S) A declaration has been made without a type specification.
"inc/isllist.h", line 9.8: 1540-022: (S) "isllist" was previously declared as "type name".
"inc/isllist.h", line 9.1: 1540-377: (I) "isllist" is declared on line 6 of "/u/crack/abuse/inc/isllist.h".
"inc/isllist.h", line 16.10: 1540-016: (S) protected member "isllist<tcpip_protocol::RequestItem*>::list_node *" cannot be accessed.
*/
