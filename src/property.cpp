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
#include <string.h>

#include "common.h"

#include "property.h"
#include "dprint.h"
#include "game.h"

class property
{
  public :
  char *name;
  char *def_str;
  int def_num;
  property(char const *Name, int Def)
  { name = strdup(Name);
    def_num = Def;
    def_str = NULL;
    next = NULL;
  }

  property(char const *Name, char const *Def)
  { name = strdup(Name);
    def_str = strdup(Def);
    next = NULL;
  }

  void set(int x)
  { if (def_str)
    {
      free(def_str);
      def_str=NULL;
    }
    def_num=x;
  }

  void set(char const *x)
  {
    if (def_str)
    {
      free(def_str);
      def_str=NULL;
    }
    def_str = strdup(x);
  }

  ~property()
  {
    if (def_str)
      free(def_str);
    free(name);
  }
  property *next;
} ;

property *property_manager::find(char const *name)
{
  for (property *i=first; i; i=i->next)
    if (!strcmp(i->name,name))
      return i;
  return NULL;
}


property_manager::~property_manager()
{
  while (first)
  {
    property *i=first;
    first=first->next;
    delete i;
  }
}

int property_manager::get(char const *name, int def)
{
  property *f=find(name);
  if (!f || f->def_str)
    return def;
  else return f->def_num;
}


char const *property_manager::get(char const *name,char const *def)
{
  property *f=find(name);
  if (!f || !f->def_str)
    return def;
  else return f->def_str;
}


void property_manager::set(char const *name, double def)
{
  property *f=find(name);
  if (f)
    f->set((int)def);
  else
  {
    f=new property(name,(int)def);
    f->next=first;
    first=f;
  }
}

void property_manager::set(char const *name, char const *def)
{
  property *f=find(name);
  if (f)
    f->set(def);
  else
  {
    f=new property(name,def);
    f->next=first;
    first=f;
  }
}


void property_manager::save(char const *filename)
{
  FILE *fp=open_FILE(filename,"wb");
  if (!fp)
    dprintf("Error opening %s to save properties\n",filename);
  else
  {
    for (property *i=first; i; i=i->next)
    {
      fprintf(fp,"%s = ",i->name);
      if (i->def_str)
        fprintf(fp,"\"%s\"\n",i->def_str);
      else
        fprintf(fp,"%d\n",i->def_num);
    }
    fclose(fp);
  }
}


void property_manager::load(char const *filename)
{
  char buf[100],*c1,*c2,name[100],str[100];
  FILE *fp=open_FILE(filename,"rb");
  if (fp)
  {
    while (!feof(fp))
    {
      if (fgets(buf,100,fp))
      {
    for (c1=buf,c2=name; *c1 && *c1!='='; c1++,c2++)
      *c2=*c1;
    if (*c1==0) { fprintf(stderr,"Missing = for property line %s in file %s\n",buf,filename);
              exit(1); }
    *c2=' ';
    while (*c2==' ') { *c2=0; c2--; }
    c1++; while (*c1==' ') c1++;
    if (*c1=='"')
    { c1++;
      for (c2=str; *c1 && *c1!='"'; c1++,c2++)
        *c2=*c1;
      *c2=0;
      if (*c1!='"') { fprintf(stderr,"Missing \" for property name %s in file %s\n",name,filename);
              exit(1); }
      set(name,str);
    } else
    {
      double x;
      if (sscanf(c1,"%lg",&x))
        set(name,x);
      else
      {
        fprintf(stderr,"Bad number/string for property name %s in file %s\n",name,filename);
        exit(1);
      }
    }
      }
    }
  }
}
