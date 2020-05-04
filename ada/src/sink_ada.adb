------------------------------------------------------------------------------------------
--
--  Purpose: Demonstrate a simple sink for UDP messages
--
------------------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with GNAT.Command_line;     use GNAT.Command_line;

with UDP;
with Signal_Handling;

procedure Sink_Ada is

  ----------------------------------------
  --  Tasks/Protected
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
      port:   Natural;
    end record;

  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  function Parse_Args return Arguments is
    parsed_args: Arguments;
  begin
    parsed_args.port := Natural'Value(Get_Argument);
    return parsed_args;
  end Parse_Args;
  args:    Arguments := Parse_Args;


  ----------------------------------------
  --  Display
  ----------------------------------------
  procedure Print_Msg(msg: in String) is
    substr_begin: Integer := msg'First;
    substr_end:   Integer := msg'First;
    search_pattern: String := ";;;;";
  begin
    while substr_begin < msg'Last loop
      substr_end := Index(msg, search_pattern, substr_begin);
      Put_Line(msg(substr_begin..substr_end-1));
      substr_begin := substr_end + search_pattern'Length;
    end loop;
  end Print_Msg;

  ----------------------------------------
  -- Task/Protected Bodies
  ----------------------------------------

  ----------------------------------------------------------------------------
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

  ----------------------------------------------------------------------------
  task body Main_Loop is
    msgs_num: Natural := 0;
    sockfd:  Integer   := 0;
  begin
    accept Start(sock: Integer) do
      sockfd := sock;
    end Start;
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
        else
          exit;
        end if;
      end;
    end loop;
    Program_Status.Set_Finished;
  end Main_Loop;

  ----------------------------------------------------------------------------
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
  Put_Line("Sink_Ada");
  New_Line;
  Put_Line("Data Port: " & Trim(args.port'Image, Ada.Strings.Left));
  --  Initialize Socket
  sockfd := UDP.get_socket;
  if sockfd < 0 then
    Put_Line("Error: Could not acquire socket.");
    return;
  elsif UDP.bind_socket(sockfd, UDP.INADDR_ANY, args.port) < 0 then
    Put_Line("Error: Could not bind to socket.");
    return;
  else
    Put_Line("Initialized Socket.");
  end if;
  New_Line;
  --  Start Main Loop and Wait
  Main_Loop.Start(sockfd);
  Program_Status.Wait;
  --  Clean up tasks and socket
  abort Main_Loop;
  abort Signal_Handler;
  if 0 = UDP.close_socket( sockfd ) then
    Put_Line("Closed Socket.");
  else
    Put_Line("Error: Could not close socket.");
  end if;
end Sink_Ada;
