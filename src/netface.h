/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

// structure used to comminicate with the engine

#ifndef __NETFACE_HPP_
#define __NETFACE_HPP_

#include <stdint.h>

#define PACKET_MAX_SIZE 1024    // this is a game data packet (udp/ipx)
#define READ_PACKET_SIZE 1024   // this is a file service packet (tcp/spx)
#define NET_CRC_FILENAME "#net_crc"
#define NET_STARTFILE    "netstart.spe"

#include <string.h>


// list of commands for general networking and file services

enum { NFCMD_OPEN,
       NFCMD_CLOSE,
       NFCMD_READ,
       NFCMD_WRITE,
       NFCMD_SEEK,
       NFCMD_SIZE,
       NFCMD_TELL,
       NFCMD_SET_FS,            // used to set the default (active) filesever
       NFCMD_CRCS_CALCED,       // engine sends this to driver after crcs are saved
       NFCMD_REQUEST_LSF,       // engine sends to driver with remote server name, returns 0 for failure or lsf name
       NFCMD_PROCESS_LSF,       // remote engine sends to driver with lsf name, when get_lsf is set in base_mem
       NFCMD_REQUEST_ENTRY,     // sent from joining client engine to driver, who then connects as client_abuse
       NFCMD_BECOME_SERVER,
       NFCMD_BLOCK,             // used by UNIX version to have engine give it up it's time-slice
       NFCMD_RELOAD_START,
       NFCMD_RELOAD_END,
       NFCMD_SEND_INPUT,
       NFCMD_INPUT_MISSING,     // when engine is waiting for input and suspects packets are missing
       NFCMD_KILL_SLACKERS,     // when the user decides the clients are taking too long to respond
       EGCMD_DIE
     };

// client commands
enum { CLCMD_JOIN_FAILED,
       CLCMD_JOIN_SUCCESS,
       CLCMD_RELOAD_START,           // will you please load netstart.spe
       CLCMD_RELOAD_END,            // netstart.spe has been loaded, please continue
       CLCMD_REQUEST_RESEND,        // input didn't arrive, please resend
       CLCMD_UNJOIN                 // causes server to delete you (addes your delete command to next out packet)
     } ;


// return codes for NFCMD_OPEN
enum { NF_OPEN_FAILED,
       NF_OPEN_LOCAL_FILE,      // should return path to local file as well
       NF_OPEN_REMOTE_FILE } ;  // returned to engine for a filename


// types of clients allowed to connect
enum { CLIENT_NFS=50,           // client can read one remote files
       CLIENT_ABUSE,            // waits for entry into a game
       CLIENT_CRC_WAITER,       // client waits for crcs to be saved
       CLIENT_LSF_WAITER        // waits for lsf to be transmitted

     } ;

// base->input_state will be one of the following

enum { INPUT_COLLECTING,       // waiting for driver to receive input from clients/server
       INPUT_PROCESSING,       // waiting for engine to process input from last tick
       INPUT_RELOAD,           // server is waiting on clients to reload, process game packets, but don't store them
       INPUT_NET_DEAD };       // net driver detected an unrecoverable net error, engine should shut down net services



// the net driver should not use any of these except SCMD_DELETE_CLIENT (0) because
// they are subject to change
enum {
       SCMD_DELETE_CLIENT,
       SCMD_VIEW_RESIZE,
       SCMD_SET_INPUT,
       SCMD_WEAPON_CHANGE,
       SCMD_END_OF_PACKET,
       SCMD_RELOAD,
       SCMD_KEYPRESS,
       SCMD_KEYRELEASE,
       SCMD_EXT_KEYPRESS,
       SCMD_EXT_KEYRELEASE,
       SCMD_CHAT_KEYPRESS,
       SCMD_SYNC
     };


struct join_struct
{
  int client_id;
  char name[100];
  join_struct *next;
} ;

struct net_packet
{
  uint8_t data[PACKET_MAX_SIZE];
  int packet_prefix_size()                 { return 5; }    // 2 byte size, 2 byte check sum, 1 byte packet order
  uint16_t packet_size()             { uint16_t size; memcpy(&size, data, sizeof(size)); return lstl(size); }
  uint8_t tick_received()            { return data[4]; }
  void set_tick_received(uint8_t x)  { data[4]=x; }
  uint8_t *packet_data()             { return data+packet_prefix_size(); }
  uint16_t get_checksum()            { uint16_t cs=*((uint16_t *)data+1); return lstl(cs); }
  uint16_t calc_checksum()
  {
    *((uint16_t *)data+1)=0;
    int i,size=packet_prefix_size()+packet_size();
    uint8_t c1=0,c2=0,*p=data;
    for (i=0; i<size; i++,p++)
    {
      c1+=*p;
      c2+=c1;
    }
    uint16_t cs=( (((uint16_t)c1)<<8) | c2);
    *((uint16_t *)data+1)=lstl(cs);
    return cs;
  }


  void packet_reset()    { set_packet_size(0); }     // 2 bytes for size, 1 byte for tick

  void add_to_packet(void *buf, int size)
  {
    if (size && size+packet_size()+packet_prefix_size()<PACKET_MAX_SIZE)
    {
      memcpy(data+packet_size()+packet_prefix_size(),buf,size);
      set_packet_size(packet_size()+size);
    }
  }
  void write_uint8(uint8_t x) { add_to_packet(&x,1); }
  void write_uint16(uint16_t x) { x=lstl(x); add_to_packet(&x,2); }
  void write_uint32(uint32_t x) { x=lltl(x); add_to_packet(&x,4); }

  void set_packet_size(uint16_t x) { uint16_t tmp = lstl(x); memcpy(data, &tmp, sizeof(tmp)); }


} ;

struct base_memory_struct
{
  net_packet packet,                        // current tick data
             last_packet;                   // last tick data (in case a client misses input, we can resend)

  int16_t mem_lock;
  int16_t calc_crcs;
  int16_t get_lsf;
  int16_t wait_reload;
  int16_t need_reload;
  int16_t input_state;          // COLLECTING or PROCESSING
  int16_t current_tick;         // set by engine, used by driver to confirm packet is not left over

  join_struct *join_list;
} ;



#endif
