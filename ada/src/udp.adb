with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;

use Interfaces;

package body UDP is

  --------------------------------------------------------------------------------
  function get_socket return Integer is
  begin
    return Integer(c_get_socket);
  end get_socket;

  --------------------------------------------------------------------------------
  function get_broadcast_socket return Integer is
  begin
    return Integer(c_get_broadcast_socket);
  end get_broadcast_socket;

  --------------------------------------------------------------------------------
  function bind_socket(sockfd:  in Integer;
                       ip_host: in String;
                       ip_port: in Integer
                      ) return Integer is
    c_sockfd:  C.int               := C.int(sockfd);
    c_ip_host: C.Strings.chars_ptr := C.Strings.New_String(ip_host);
    c_ip_port: C.unsigned_short    := C.unsigned_short(ip_port);
    c_return: Integer;
  begin
    c_return := Integer(c_bind_socket(c_sockfd, c_ip_host, c_ip_port));
    C.Strings.Free(c_ip_host);
    return c_return;
  end bind_socket;

  --------------------------------------------------------------------------------
  function send_msg(sockfd:  in Integer;
                    ip_host: in String;
                    ip_port: in Natural;
                    msg:     in String;
                    msg_len: in Natural
                   ) return Integer is
    c_sockfd:  C.int               := C.int(sockfd);
    c_ip_host: C.Strings.chars_ptr := C.Strings.New_String(ip_host);
    c_ip_port: C.unsigned_short    := C.unsigned_short(ip_port);
    c_msg:     C.Strings.chars_ptr := C.Strings.New_String(msg);
    c_msg_len: C.unsigned          := C.unsigned(msg_len);
    c_return: Integer;
  begin
    c_return := Integer(c_send_msg(c_sockfd, c_ip_host, c_ip_port, c_msg, c_msg_len));
    C.Strings.Free(c_ip_host);
    C.Strings.Free(c_msg);
    return c_return;
  end send_msg;

  --------------------------------------------------------------------------------
  function broadcast_msg(sockfd:  in     Integer;
                         ip_host: in     String;
                         ip_port: in     Natural;
                         msg:     in     String;
                         msg_len: in     Natural
                        ) return Integer is
    c_sockfd:  C.int            := C.int(sockfd);
    c_ip_host: C.char_array     := C.To_C(ip_host);
    c_ip_port: C.unsigned_short := C.unsigned_short(ip_port);
    c_msg:     C.char_array     := C.To_C(msg);
    c_msg_len: C.unsigned       := C.unsigned(msg_len);
    c_return: Integer;
  begin
    c_return := Integer(c_broadcast_msg(c_sockfd, c_ip_host, c_ip_port, c_msg, c_msg_len));
    --C.Strings.Free(c_ip_host);
    --C.Strings.Free(c_msg);
    return c_return;
  end broadcast_msg;

  --------------------------------------------------------------------------------
  function recv_msg(sockfd:  in     Integer;
                    msg:        out String;
                    msg_len:    out Natural;
                    ip_host:    out String;
                    ip_port:    out Natural
                   ) return Integer is
    tmp_msg:     String(1..4096) := (others => Character'Val(0));
    tmp_ip_host: String(1..15)   := (others => Character'Val(0));
    c_sockfd:  C.int               := C.int(sockfd);
    c_msg:     C.Strings.chars_ptr := C.Strings.New_String(tmp_msg);
    c_msg_len: C.unsigned;
    c_ip_host: C.Strings.chars_ptr := C.Strings.New_String(tmp_ip_host);
    c_ip_port: C.unsigned_short;
    c_return: Integer;
  begin
    c_return := Integer(c_recv_msg(c_sockfd, c_msg, c_msg_len, c_ip_host, c_ip_port));
    msg     := C.Strings.Value(c_msg);
    msg_len := Natural(c_msg_len);
    ip_host := C.Strings.Value(c_ip_host);
    ip_port := Natural(c_ip_port);
    C.Strings.Free(c_ip_host);
    C.Strings.Free(c_msg);
    return c_return;
  end recv_msg;

  --------------------------------------------------------------------------------
  procedure Split_IP (ip_addr: in     String;
                      ip_host:    out String;
                      ip_port:    out Natural) is
    first, last, split: Natural;
  begin
    first := ip_addr'First;
    last  := ip_addr'Last;
    split := Index(ip_addr, ":");
    move(ip_addr(first..split-1), ip_host);
    ip_port := Natural'Value(ip_addr(split+1..last));
  end Split_IP;



end UDP;
