with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
use Ada.Strings;
with GNAT.Command_line;

with UDP;

procedure Sink_Ada is
  use GNAT.Command_line;
  -----------------------------------------------------
  type Arguments is
    record
      port:   Natural;
    end record;
  -----------------------------------------------------
  function Parse_Args return Arguments is
    parsed_args: Arguments;
  begin
    parsed_args.port := Natural'Value(Get_Argument);
    return parsed_args;
  end Parse_Args;
  -----------------------------------------------------
  procedure Print_Msg(msg: in String) is
    substr_begin: Integer := msg'First;
    substr_end:   Integer := msg'First;
    search_pattern: String := ";;;;";
  begin
    while substr_begin < msg'Last loop
      substr_end := Index(msg, search_pattern, substr_begin);
      Put_Line(msg(substr_begin..substr_end));
      substr_begin := substr_end + search_pattern'Length;
    end loop;
  end Print_Msg;
  -----------------------------------------------------
  args:     Arguments := Parse_Args;
  sockfd:   Integer := 0;
  msgs_num: Natural := 0;
begin
  Put_Line("Data Port: " & Trim(args.port'Image, Ada.Strings.Left));
  sockfd := UDP.get_socket;
  if sockfd < 0 then
    Put_Line("Error: Could not acquire socket.");
    return;
  end if;
  if UDP.bind_socket(sockfd, UDP.INADDR_ANY, args.port) < 0 then
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
    begin
      if UDP.recv_msg(sockfd, msg, msg_len, recv_host, recv_port) > 0 then
        msgs_num := msgs_num + 1;
        Print_Msg(msg(msg'First..msg_len));
        Put_Line("Ada Sink msg #"& Trim(msgs_num'Image, Left));
        Put_Line("Ada Sink "& UDP.INADDR_ANY &":"& Trim(args.port'Image, Left)
                &" <= "& Trim(recv_host, Right) &":"& Trim(recv_port'Image, Left));
        New_Line;
      end if;
    end;
  end loop;

end Sink_Ada;
