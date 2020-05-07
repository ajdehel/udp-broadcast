------------------------------------------------------------------------------------------
--
--  Purpose: Demonstrate a simple UDP broadcast client that additionally sends messages
--
------------------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with GNAT.Command_line;     use GNAT.Command_line;

with Signal_Handling;
with UDP;

procedure Client_Ada is

  ----------------------------------------
  --  Tasks
  ----------------------------------------
  task Main_Loop is
    entry Start(sock: Integer);
  end Main_Loop;
  task Signal_Handler is
    entry Start;
  end Signal_Handler;

  protected Program_Status is
    procedure Set_Finished;
    entry Wait;
    private
    finished: Boolean := False;
  end Program_Status;

  ----------------------------------------
  --  Argument Handling
  ----------------------------------------
  type Arguments is
    record
      listener_port:   Natural;
      sink_host:   String(1..16) := (others => Character'Val(0));
      sink_port:   Natural;
      instance_id: Natural;
    end record;

  function Parse_Args return Arguments is
    parsed_args: Arguments;
  begin
    parsed_args.listener_port := Natural'Value(Get_Argument);
    UDP.Split_IP( Get_Argument, parsed_args.sink_host, parsed_args.sink_port );
    parsed_args.instance_id := Natural'Value(Get_Argument);
    return parsed_args;
  end Parse_Args;

  args:     Arguments := Parse_Args;

  ----------------------------------------
  -- Task Bodies
  ----------------------------------------

  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  protected body Program_Status is
    procedure Set_Finished is
    begin
      finished := True;
    end Set_Finished;
    entry Wait when finished = True is
    begin
      finished := False;
    end Wait;
  end Program_Status;

  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  task body Main_Loop is
    msgs_num:    Natural := 0;
    sockfd:      Integer := 0;
  begin
    accept Start(sock: in Integer) do
      sockfd := sock;
    end Start;
    --  LOOP
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
          if bytes_sent = Length(buffer) then
            Put_Line("Msg "& msgs_num'Image &" sent");
          else
            Put_Line("Error sending UDP message. " &bytes_sent'Image &"!="& Length(buffer)'Image);
            exit;
          end if;
        else
          Put_Line("Error sending UDP message. " &bytes_sent'Image &"!="& Length(buffer)'Image);
          exit;
        end if;
      end;
    end loop;
    Program_Status.Set_Finished;
  end Main_Loop;

  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  task body Signal_Handler is
  begin
    accept Start do
      Signal_Handling.Add_SIGINT;
    end Start;
    Signal_Handling.Wait;
    New_Line;
    Put_Line("Caught Signal.");
    Program_Status.Set_Finished;
  end Signal_Handler;

----------------------------------------
-- Procedure
----------------------------------------
sockfd:  Integer   := 0;
begin
  Signal_Handler.Start;
  Put_Line("Client_Ada");
  New_Line;
  Put_Line("Instance:  " & Trim(args.instance_id'Image, Ada.Strings.Left));
  Put_Line("Data Port: " & Trim(args.listener_port'Image, Ada.Strings.Left));
  Put_Line("Sink Host: " & Trim(args.sink_host, Ada.Strings.Left));
  Put_Line("Sink Port: " & Trim(args.sink_port'Image, Ada.Strings.Left));
  --  Initialize Socket
  sockfd := UDP.get_broadcast_socket;
  if sockfd < 0 then
    Put_Line("Error: Could not acquire socket.");
    return;
  elsif UDP.bind_socket(sockfd, UDP.INADDR_ANY, args.listener_port) < 0 then
    Put_Line("Error: Could not bind to socket.");
    return;
  else
    Put_Line("Initialized Socket.");
  end if;
  New_Line;
  --  Start Main Loop and Wait
  Main_Loop.Start(sockfd);
  -- Wait on Program_Status
  Program_Status.Wait;
  --  Clean up tasks and socket
  abort Main_Loop;
  abort Signal_Handler;
  if 0 = UDP.close_socket( sockfd ) then
    Put_Line("Closed Socket.");
  else
    Put_Line("Error: Could not close socket.");
  end if;
end Client_Ada;
