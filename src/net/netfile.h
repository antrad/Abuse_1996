#ifndef __NETFILE_HPP_
#define __NETFILE_HPP_

#include <sys/types.h>


void secure_filename(char *filename, char *mode);

class nfs_client    // this is a client only read's a file
{
  public :
  int socket_fd;
  int file_fd;

  long size_to_read;
  long size;
  nfs_client *next;
  nfs_client(int Socket_fd, int File_fd, nfs_client *Next);
  int send_read();     // flushes as much of size_to_read as possible
  ~nfs_client();
} ;


class remote_file    // a remote client has opened this file with us
{
  public :
  int socket_fd;
  void r_close(char *reason);
  long size;   // server tells us the size of the file when we open it
  int open_local;
  remote_file *next;
  remote_file(char *filename, char *mode, remote_file *Next);

  int unbuffered_read(int out_fd, size_t count);
  int unbuffered_write(void *buf, size_t count) { return 0; } // not supported
  int unbuffered_tell();
  int unbuffered_seek(long offset);
  int file_size() { return size; }
  int open_failure() { return socket_fd<0; }
  ~remote_file();
  int fd() { return socket_fd; }
} ;


extern nfs_client *first_nfs_client;
extern remote_file *remote_file_list;
extern char default_fs_name[256];

int process_nfs_command(nfs_client *c);
void add_nfs_client(int fd);
int local_address(char *server_name);    // returns 1 if server name is ourself
remote_file *find_rfile(int fd);
void unlink_remote_file(remote_file *rf);
int open_file(char *&filename, char *mode);
int fetch_crcs(char *server);

#endif
