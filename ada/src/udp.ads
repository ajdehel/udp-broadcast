with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

use Interfaces;

package UDP is

  INADDR_ANY: constant String := "0.0.0.0";


  --------------------------------------------------------------------------------
  --  function: get_socket
  --  params:
  --    None
  --  returns:
  --    A socket file descriptor (a positive int)
  --
  --  Notes:
  --    This function passes through to a C++ function
  function get_socket return Integer;
  function c_get_socket return C.int
      with Import => True, Convention => C, External_Name => "get_socket";


  --------------------------------------------------------------------------------
  --  function: get_broadcast_socket
  --  params:
  --    None
  --  returns:
  --    A socket file descriptor (a positive int)
  --
  --  Notes:
  --    This function passes through to a C++ function
  function get_broadcast_socket return Integer;
  function c_get_broadcast_socket return C.int
      with Import => True, Convention => C, External_Name => "get_broadcast_socket";


  --------------------------------------------------------------------------------
  --  function: bind_socket
  --  params:
  --    (in)     sockfd:  Integer => a socket file descriptor
  --    (in)     ip_host: String  => an IPv4 address (e.g. "127.0.0.1")
  --    (in)     ip_port: Natural => a port number
  --  returns:
  --    0 on success
  --
  --  Notes:
  --    This function passes through to a C++ function to handle the networking
  function bind_socket(sockfd:  in Integer;
                       ip_host: in String;
                       ip_port: in Natural
                      ) return Integer;
  function c_bind_socket(sockfd:  in out C.int;
                         ip_host: in out C.char_array;
                         ip_port: in     C.unsigned_short
                        ) return C.int
      with Import => True, Convention => C, External_Name => "bind_socket";


  --------------------------------------------------------------------------------
  --  function: send_msg
  --  params:
  --    (in)     sockfd:  Integer => a socket file descriptor
  --    (in)     ip_host: String  => the IPv4 address (e.g. "127.0.0.1") to send the
  --                        message to
  --    (in)     ip_port: Natural => the port to send the message to
  --    (in)     msg:     String  => the message
  --    (in)     msg_len: Natural => the length of the message
  --  returns:
  --    0 on success
  --
  --  Notes:
  --    This function passes through to a C++ function to handle the networking
  function send_msg(sockfd:  in Integer;
                    ip_host: in String;
                    ip_port: in Natural;
                    msg:     in String;
                    msg_len: in Natural
                   ) return Integer;
  function c_send_msg(sockfd:  in     C.int;
                      ip_host: in out C.char_array;
                      ip_port: in     C.unsigned_short;
                      msg:     in out C.char_array;
                      msg_len: in     C.unsigned
                     ) return C.int
      with Import => True, Convention => C, External_Name => "send_msg";


  --------------------------------------------------------------------------------
  --  function: broadcast_msg
  --  params:
  --    (in)     sockfd:  Integer => a socket file descriptor
  --    (in)     ip_host: String  => the broadcast IPv4 address (e.g. "127.0.0.1") to
  --                        send the message to
  --    (in)     ip_port: Natural => the port to broadcast the message on
  --    (in)     msg:     String  => the message
  --    (in)     msg_len: Natural => the length of the message
  --  returns:
  --    0 on success
  --
  --  Notes:
  --    This function passes through to a C++ function to handle the networking
  function broadcast_msg(sockfd:  in Integer;
                         ip_host: in String;
                         ip_port: in Natural;
                         msg:     in String;
                         msg_len: in Natural
                        ) return Integer;
  function c_broadcast_msg(sockfd:  in     C.int;
                           ip_host: in out C.char_array;
                           ip_port: in     C.unsigned_short;
                           msg:     in out C.char_array;
                           msg_len: in     C.unsigned
                          ) return C.int
      with Import => True, Convention => C, External_Name => "broadcast_msg";


  --------------------------------------------------------------------------------
  --  function: recv_msg
  --  params:
  --    (in)     sockfd:  Integer => a socket file descriptor
  --    (in out) msg:     String  => the message received
  --       (out)     msg_len: Natural => the length of the message received
  --    (in out) ip_host: String  => the IPv4 address (e.g. "127.0.0.1") that sent the
  --       (out)                     message
  --    ip_port: Natural => the port that the message was sent on
  --  returns:
  --    the number of bytes received from the socket
  --
  --  Notes:
  --    This function passes through to a C++ function to handle the networking
  function recv_msg(sockfd:  in     Integer;
                    msg:     in out String;
                    msg_len:    out Natural;
                    ip_host: in out String;
                    ip_port:    out Natural
                   ) return Integer;
  function c_recv_msg(sockfd:  in     C.int;
                      msg:        out C.char_array;
                      msg_len:    out C.unsigned;
                      ip_host:    out C.char_array;
                      ip_port:    out C.unsigned_short
                     ) return C.int
      with Import => True, Convention => C, External_Name => "recv_msg";


  --------------------------------------------------------------------------------
  --  procedure: Split_IP
  --  params:
  --    (in)     ip_addr: String  => an IPv4 address with port number (e.g. "127.0.0.1":12345)
  --       (out) ip_host: String  => the IPv4 address part (i.e. "127.0.0.1")
  --       (out) ip_port: Natural => the Port number part (i.e. 12345)
  procedure Split_IP (ip_addr: in     String;
                      ip_host:    out String;
                      ip_port:    out Natural);


end UDP;
