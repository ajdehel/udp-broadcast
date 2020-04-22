with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Command_line;

with UDP;

procedure Server_Ada is
  use GNAT.Command_line;
  -----------------------------------------------------
  type Arguments is
    record
      broadcast_host: String(1..16) := (others => Character'Val(0));
      broadcast_port: Natural;
      instance_id:    Natural;
      period:         Float;
    end record;
  -----------------------------------------------------
  function Parse_Args return Arguments is
    parsed_args: Arguments;
  begin
    loop
      case GetOpt("p:") is
        when 'p' =>
          parsed_args.period := Float'Value(Parameter);
        when others =>
          exit;
      end case;
    end loop;
    UDP.Split_IP( Get_Argument, parsed_args.broadcast_host, parsed_args.broadcast_port );
    parsed_args.instance_id := Natural'Value(Get_Argument);
    return parsed_args;
  end Parse_Args;
  -----------------------------------------------------
  args:     Arguments := Parse_Args;
  sockfd:   Integer := 0;
  msgs_num: Natural := 0;
begin
  Put_Line("Instance:       " & Trim(args.instance_id'Image, Ada.Strings.Left));
  Put_Line("Broadcast Host: " & Trim(args.broadcast_host, Ada.Strings.Left));
  Put_Line("Broadcast Port: " & Trim(args.broadcast_port'Image, Ada.Strings.Left));
  Put_Line("Period:         " & Trim(args.period'Image, Ada.Strings.Left));
  sockfd := UDP.get_broadcast_socket;
  if Integer(sockfd) < 0 then
    Put_Line("Error: Could not bind to socket.");
    return;
  end if;
  loop
    msgs_num := msgs_num + 1;
    declare
      msg: string :=
        "ada Server["& Trim(args.instance_id'Image, Ada.Strings.Left) &"] "
        & "msg #"& Trim(msgs_num'Image, Ada.Strings.Left) &";;;;"
        & "ada Server["& Trim(args.instance_id'Image, Ada.Strings.Left) &"] 0:0"
        & " => "& Trim(args.broadcast_host, Ada.Strings.Right) &":"
        & Trim(args.broadcast_port'Image, Ada.Strings.Left) &";;;;";
      msg_len: Positive := msg'Length;
      bytes_sent: Integer := 0;
    begin
      delay Duration(args.period);
      bytes_sent := UDP.broadcast_msg(sockfd,
                                      Trim(args.broadcast_host, Ada.Strings.Right),
                                      args.broadcast_port,
                                      msg,
                                      msg_len);
      if bytes_sent = msg'Length then
        Put_Line("Msg "& msgs_num'Image &" sent");
      else
        Put_Line("Error sending UDP message.");
      end if;

    end;
  end loop;
end Server_Ada;
