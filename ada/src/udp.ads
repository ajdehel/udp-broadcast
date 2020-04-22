with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

use Interfaces;

package UDP is

  INADDR_ANY: constant String := "0.0.0.0";

  --------------------------------------------------------------------------------
  function get_socket return Integer;
  function c_get_socket return C.int
      with Import => True, Convention => C, External_Name => "get_socket";

  --------------------------------------------------------------------------------
  function get_broadcast_socket return Integer;
  function c_get_broadcast_socket return C.int
      with Import => True, Convention => C, External_Name => "get_broadcast_socket";

  --------------------------------------------------------------------------------
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

  procedure Split_IP (ip_addr: in     String;
                      ip_host:    out String;
                      ip_port:    out Natural);

end UDP;
