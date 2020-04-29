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

  --------------------------------------------------------------------------------
  --  function: close_socket
  --  params:
  --    None
  --  returns:
  --    A socket file descriptor (a positive int)
  --
  --  Notes:
  --    This function passes through to a C++ function
  function close_socket(sockfd: in Integer) return Integer;

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
