/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __SPECS_HPP_
#define __SPECS_HPP_

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "linked.h"

extern char const *spec_types[];

enum
{
    SPEC_INVALID_TYPE = 0,
    SPEC_COLOR_TABLE = 1,
    SPEC_PALETTE = 2,
    /* Empty slot */
    SPEC_IMAGE = 4,
    SPEC_FORETILE = 5,
    SPEC_BACKTILE = 6,
    SPEC_CHARACTER = 7,
    SPEC_MORPH_POINTS_8 = 8,
    SPEC_MORPH_POINTS_16 = 9,
    SPEC_GRUE_OBJS = 10,
    SPEC_EXTERN_SFX = 11,
    SPEC_DMX_MUS = 12,
    SPEC_PATCHED_MORPH = 13,
    SPEC_NORMAL_FILE = 14,
    SPEC_COMPRESS1_FILE = 15,
    SPEC_VECTOR_IMAGE = 16,
    SPEC_LIGHT_LIST = 17,
    SPEC_GRUE_FGMAP = 18,
    SPEC_GRUE_BGMAP = 19,
    SPEC_DATA_ARRAY = 20,
    SPEC_CHARACTER2 = 21,
    SPEC_PARTICLE = 22,
    SPEC_EXTERNAL_LCACHE = 23,
};

#define SPEC_SIGNATURE    "SPEC1.0"
#define SPEC_SIG_SIZE     8

#define SPEC_FLAG_LINK    1

#define SPEC_SEARCH_INSIDE_OUTSIDE 1
#define SPEC_SEARCH_OUTSIDE_INSIDE 2
#define SPEC_SEARCH_INSIDE_ONLY    3

/*  struct spec_header
 *  {
 *      char signature[8];
 *      uint16_t entries_count;
 *      struct entry
 *      {
 *          uint8_t type;
 *          uint8_t name_length;
 *          char name[name_length];
 *          uint8_t flags;
 *          if (flags & LINK)
 *          {
 *              uint8_t filename_length;
 *              char filename[filename_length];
 *          }
 *          else
 *          {
 *              uint32_t data_size;
 *              uint32_t offset;
 *          }
 *      } entries[entries_count];
 *  }
 */

void set_spec_main_file(char const *filename, int search_order=SPEC_SEARCH_OUTSIDE_INSIDE);

void set_filename_prefix(char const *prefix);
char *get_filename_prefix();
void set_save_filename_prefix(char const *prefix);
char *get_save_filename_prefix();
#define JFILE_CLONED 1

class bFILE     // base file type which other files should be derived from (jFILE & NFS for now)
{
  protected :
  unsigned char *rbuf,*wbuf;
  unsigned long rbuf_start,rbuf_end,rbuf_size,
                wbuf_end,wbuf_size;                // can't seek while writing!
  int flush_writes();                             // returns 0 on failure, else # of bytes written

  virtual int unbuffered_read(void *buf, size_t count)  = 0;
  virtual int unbuffered_write(void const *buf, size_t count) = 0;
  virtual int unbuffered_tell()                         = 0;
  virtual int unbuffered_seek(long offset, int whence)  = 0;   // whence=SEEK_SET, SEEK_CUR,
                                                               // SEEK_END, ret=0=success
  virtual int allow_read_buffering();
  virtual int allow_write_buffering();
  public :
  bFILE();
  virtual int open_failure() = 0;
  int read(void *buf, size_t count);        // returns number of bytes read, calls unbuffer_read
  int write(void const *buf, size_t count); // returns number of bytes written
  int seek(long offset, int whence);        // whence=SEEK_SET, SEEK_CUR, SEEK_END, ret=0=success
  int tell();
  virtual int file_size() = 0;

  virtual ~bFILE();

    // read and write using little-endianness
    uint16_t read_uint16();
    uint32_t read_uint32();
    uint8_t read_uint8();
    double read_double();
    void write_uint16(uint16_t x);
    void write_uint32(uint32_t x);
    void write_uint8(uint8_t x);
    void write_double(double x);
};

class jFILE : public bFILE     // this file type will use virtual opens inside of a spe
{
  char *fname;
  char *tmp_write_name;
  int access;
  int fd,flags;
  long start_offset,file_length;    // offset of file from actual file begining

  long current_offset;  // current offset

public :
    int get_fd() const { return fd; }

  void open_internal(char const *filename, char const *mode, int flags);
  void open_external(char const *filename, char const *mode, int flags);

  jFILE(char const *filename, char const *access_string);      // same as fopen parameters
  jFILE(FILE *file_pointer);                      // assumes fp is at begining of file
  virtual int open_failure() { return fd<0; }
  virtual int unbuffered_read(void *buf, size_t count);       // returns number of bytes read
  virtual int unbuffered_write(void const *buf, size_t count);     // returns number of bytes written
  virtual int unbuffered_seek(long offset, int whence);      // whence=SEEK_SET, SEEK_CUR,
                                                             // SEEK_END, ret=0=success
  virtual int unbuffered_tell();
  virtual int file_size() { return file_length; }
  virtual ~jFILE();
} ;

class spec_entry
{
public:
    spec_entry(uint8_t spec_type, char const *object_name,
               char const *link_filename,
               unsigned long data_size, unsigned long data_offset);
    ~spec_entry();

    void Print();

    char *name;
    void *data;
    unsigned long size, offset;
    uint8_t type;
};


class spec_directory
{
public :
    spec_directory(FILE *fp);
    spec_directory(bFILE *fp);
    spec_directory();
    ~spec_directory();

    void startup(bFILE *fp);
    void FullyLoad(bFILE *fp);

//  spec_directory(char *filename);  ; ; not allowed anymore, user must construct file first!
  spec_entry *find(char const *name);
  spec_entry *find(char const *name, int type);
  spec_entry *find(int type);
  long find_number(char const *name);
  long find_number(int type);
  void remove(spec_entry *e);
  void add_by_hand(spec_entry *e);
  void calc_offsets();
  long data_start_offset();  // returns the first offset past directory items
  long data_end_offset();    // this should be the end of the file
  long type_total(int type);
  jFILE *write(char const *filename);
  int    write(bFILE *fp);
  void print();
  void delete_entries();   // if the directory was created by hand instead of by file

    int total;
    spec_entry **entries;
    void *data;
    size_t size;
};

/*jFILE *add_directory_entry(char *filename,
                         unsigned short data_type,
                         char *data_name,
                         unsigned long data_size,
                         char *link_filename=NULL); */

uint16_t read_uint16(FILE *fp);
uint32_t read_uint32(FILE *fp);
uint8_t read_uint8(FILE *fp);

void write_uint16(FILE *fp, uint16_t x);
void write_uint32(FILE *fp, uint32_t x);
void write_uint8(FILE *fp, uint8_t x);

void set_spec_main_file(char *filename, int Search_order);
void set_file_opener(bFILE *(*open_fun)(char const *, char const *));
void set_no_space_handler(void (*handle_fun)());
bFILE *open_file(char const *filename, char const *mode);
#endif

