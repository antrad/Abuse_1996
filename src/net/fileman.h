#ifndef __FILEMAN_HPP_
#define __FILEMAN_HPP_

#include <unistd.h>
#include "sock.h"
#include <stdlib.h>
#include <string.h>


class file_manager
{
  net_address *default_fs;
  int no_security;

  class nfs_client
  {
    public :
    net_socket *sock;
    int file_fd;

    nfs_client *next;
    int32_t size_to_read;
    int32_t size;
    nfs_client(net_socket *sock, int file_fd, nfs_client *next);
    int send_read();     // flushes as much of size_to_read as possible
    ~nfs_client();
  } ;


  class remote_file    // a remote client has opened this file with us
  {
    public :
    net_socket *sock;
    void r_close(char const *reason);
    int32_t size;   // server tells us the size of the file when we open it
    int open_local;
    remote_file *next;
    remote_file(net_socket *sock, char const *filename, char const *mode, remote_file *Next);

    int unbuffered_read(void *buffer, size_t count);
    int unbuffered_write(void const *buf, size_t count) { return 0; } // not supported
    int32_t unbuffered_tell();
    int32_t unbuffered_seek(int32_t offset);
    int32_t file_size() { return size; }
    int open_failure() { return sock==NULL; }
    ~remote_file();
    int fd() { if (sock) return sock->get_fd(); else return -1; }
  } ;


  nfs_client *nfs_list;
  remote_file *remote_list;

  int process_nfs_command(nfs_client *c);
  void secure_filename(char *filename, char *mode);
  remote_file *find_rf(int fd);
  net_protocol *proto;
  public :

  file_manager(int argc, char **argv, net_protocol *proto);
  void process_net();
  void add_nfs_client(net_socket *sock);


  int rf_open_file(char const *&filename, char const *mode);
  int32_t rf_tell(int fd);
  int32_t rf_seek(int fd, int32_t offset);
  int rf_read(int fd, void *buffer, size_t count);
  int rf_close(int fd);
  int32_t rf_file_size(int fd);
  void set_default_fs(net_address *def) { default_fs=def->copy(); }
  ~file_manager() { if (default_fs) delete default_fs; }
} ;

extern file_manager *fman;


#endif
