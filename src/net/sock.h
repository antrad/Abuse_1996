#ifndef __SOCK_HPP_
#define __SOCK_HPP_

extern const char notify_signature[];
extern const char notify_response[];

class net_address
{
public:
  enum protocol { IP, IPX, MODEM, NETBIOS, OTHER };
  virtual protocol protocol_type() const = 0;
  virtual int equal(const net_address *who) const = 0;
  int operator==(const net_address &who) { return equal(&who); }
  virtual int set_port(int port) = 0;
  virtual int get_port() = 0;
  virtual void print()               { ; }
  virtual net_address *copy()    = 0;
  virtual void store_string(char *st, int st_length) = 0;    // this should be able to be used get_node_address()
  virtual ~net_address() { ; }
};


class net_socket
{
public:
  enum socket_type { SOCKET_SECURE, SOCKET_FAST };

  virtual int error()                                              = 0;
  virtual int ready_to_read()                                      = 0;
  virtual int ready_to_write()                                     = 0;
  virtual int write(void const *buf, int size, net_address *addr=0)   = 0;
  virtual int read(void *buf, int size, net_address **addr=0)      = 0;
  virtual int get_fd()                                             = 0;
  virtual ~net_socket()              { ; }
  virtual void read_selectable()     { ; }
  virtual void read_unselectable()   { ; }
  virtual void write_selectable()    { ; }
  virtual void write_unselectable()  { ; }
  virtual int listen(int port)       { return 0; }
  virtual net_socket *accept(net_address *&from) { from=0; return 0; }
};

class net_protocol
{
public:
  enum debug_type
    { DB_OFF,                // no debug printing
      DB_MAJOR_EVENT,        // print major events
      DB_IMPORTANT_EVENT,    // anything that would tell where a lockup occurs
      DB_MINOR_EVENT };      // anything you can think of

private:
  debug_type debug_setting;
public:

  enum connect_flags
  { READ_WRITE,          // flags for connect to server
         WRITE_ONLY };


  int debug_level(debug_type min_level) { return min_level<=debug_setting; }
  void set_debug_printing(debug_type level) { debug_setting=level; }

  static net_protocol *first;
  net_protocol *next;

  virtual net_address *get_local_address() = 0;
  virtual net_address *get_node_address(char const *&server_name, int def_port, int force_port) = 0;
  virtual net_socket *connect_to_server(net_address *addr,
                    net_socket::socket_type sock_type=net_socket::SOCKET_SECURE) =0;
  virtual net_socket *create_listen_socket(int port, net_socket::socket_type sock_type) = 0;
  virtual int installed() = 0;
  virtual char const *name() = 0;
  virtual int select(int block) = 0;          // return # of sockets available for read & writing
  virtual void cleanup() { ; }                // should do any needed pre-exit cleanup stuff
  net_socket *connect_to_server(char const *&server_name, int port, int force_port=0,
                net_socket::socket_type sock_type=net_socket::SOCKET_SECURE);

    // Notification methods
    virtual net_socket *start_notify(int port, void *data, int len) { return 0; }
    virtual void end_notify() { }
    // for protocols that don't know how to find address without help, start_notify creates
    //   a fast/connectionless socket to respond to find address request packets.  After start_notify,
    //   protocol::select should automatically forward the specified data to all requesters.
    //   find_address should "broadcast" a notification request packet to all addresses it can
    //   and wait for any responses

    // Find methods
  virtual net_address *find_address(int port, char *name) { return 0; }   // name should be a 256 byte buffer
  virtual void reset_find_list() { ; }

  net_protocol() { next=first; first=this; debug_setting=DB_OFF; }
  virtual ~net_protocol() { cleanup(); }
};

#endif









