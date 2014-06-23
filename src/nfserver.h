/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#ifndef __NFSERVER_HPP_
#define __NFSERVER_HPP_

#include "specs.h"
#include "netface.h"
#include "net/sock.h"

int net_init(int argc, char **argv);
void net_uninit();
void service_net_request();
void wait_min_players();
void server_check();
void remove_client(int client_number);

int net_start();
bFILE *open_nfs_file(char const *filename, char const *mode);

int NF_open_file(char const *filename, char const *mode);
long NF_close(int fd);
long NF_read(int fd, void *buf, long size);
long NF_filelength(int fd);
long NF_tell(int fd);
long NF_seek(int fd, long offset);
int NF_set_file_server(net_address *addr);

int request_server_entry();
int server_entry_continue();
void net_reload();
void read_new_views();
int set_file_server(char const *name);
int set_file_server(net_address *addr);

int join_remote_game(char *name);
int become_server(char *name);
int get_remote_lsf(net_address *addr, char *filename);   // filename should be 256 bytes


void send_local_request();                          // sends from *base
int get_inputs_from_server(unsigned char *buf);     // return bytes read into buf (will be less than PACKET_MAX_SIZE


int client_number();
extern net_address *net_server;
extern base_memory_struct *base;   // points to shm_addr

#endif

