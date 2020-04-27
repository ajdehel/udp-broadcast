------------------------------------------------------------------------------------------
--
--  Purpose: Demonstrate a simple UDP broadcast client that additionally sends messages
--
------------------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
use Ada.Strings;
with GNAT.Command_line;

with UDP;

procedure Client_Ada is
  use GNAT.Command_line;

  ------------------------------------------------------------
  type Arguments is
    record
      listener_port:   Natural;
      sink_host:   String(1..16) := (others => Character'Val(0));
      sink_port:   Natural;
      instance_id: Natural;
    end record;

  ------------------------------------------------------------
  function Parse_Args return Arguments is
    parsed_args: Arguments;
  begin
    parsed_args.listener_port := Natural'Value(Get_Argument);
    UDP.Split_IP( Get_Argument, parsed_args.sink_host, parsed_args.sink_port );
    parsed_args.instance_id := Natural'Value(Get_Argument);
    return parsed_args;
  end Parse_Args;

  ------------------------------------------------------------
  args:     Arguments := Parse_Args;
  sockfd:   Integer := 0;
  msgs_num: Natural := 0;

begin
  Put_Line("Instance:  " & Trim(args.instance_id'Image, Ada.Strings.Left));
  Put_Line("Data Port: " & Trim(args.listener_port'Image, Ada.Strings.Left));
  Put_Line("Sink Host: " & Trim(args.sink_host, Ada.Strings.Left));
  Put_Line("Sink Port: " & Trim(args.sink_port'Image, Ada.Strings.Left));
  sockfd := UDP.get_broadcast_socket;

  if sockfd < 0 then
    Put_Line("Error: Could not acquire socket.");
    return;
  end if;

  if UDP.bind_socket(sockfd, UDP.INADDR_ANY, args.listener_port) < 0 then
    Put_Line("Error: Could not bind to socket.");
    return;
  end if;

  loop
    declare
      buffer:     Unbounded_String;
      msg:        String(1..4096);
      msg_len:    Natural;
      recv_host:  String(1..16);
      recv_port:  Natural;
      bytes_sent: Natural;
    begin
      if UDP.recv_msg(sockfd, msg, msg_len, recv_host, recv_port) > 0 then
        msgs_num := msgs_num + 1;
        --  From Server
        Append(buffer, msg(1..msg_len));
        --  1st Part
        Append(buffer,
          "Ada Client["& Trim(args.instance_id'Image, Left) &"] "
          & "msg #"&Trim(msgs_num'Image, Left)&";;;;");
        --  2nd Part
        Append(buffer,
          "Ada Client["& Trim(args.instance_id'Image, Left) &"] "
          & UDP.INADDR_ANY&":"&Trim(args.listener_port'Image, Left)
          & " <= "
          & Trim(recv_host, Right)&":"&Trim(recv_port'Image, Left)&";;;;");
        --  3rd Part
        Append(buffer,
          "Ada Client["& Trim(args.instance_id'Image, Left) &"] "
          & UDP.INADDR_ANY&":"&Trim(args.listener_port'Image, Left)
          & " => "
          & Trim(args.sink_host, Right)&":"&Trim(args.sink_port'Image, Left)&";;;;");
        bytes_sent := UDP.send_msg(sockfd,
                                   Trim(args.sink_host, Ada.Strings.Right),
                                   args.sink_port,
                                   To_String(buffer),
                                   Length(buffer));
        if bytes_sent = msg'Length then
          Put_Line("Msg "& msgs_num'Image &" sent");
        else
          Put_Line("Error sending UDP message.");
        end if;
      end if;
    end;
  end loop;

end Client_Ada;
