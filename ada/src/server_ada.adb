------------------------------------------------------------------------------------------
--
--  Purpose: Demonstrate a simple UDP broadcast server
--
------------------------------------------------------------------------------------------

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Task_Identification; use Ada.Task_Identification;
with GNAT.Command_line;       use GNAT.Command_line;

with Signal_Handling;
with UDP;

procedure Server_Ada is

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
      broadcast_host: String(1..16) := (others => Character'Val(0));
      broadcast_port: Natural;
      instance_id:    Natural;
      period:         Float := 1.0;
    end record;

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

  args:   Arguments := Parse_Args;

  ----------------------------------------
  -- Task Bodies
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
    msgs_num:    Natural := 0;
    sockfd:      Integer := 0;
  begin
    accept Start(sock: in Integer) do
      sockfd := sock;
    end Start;
    --  LOOP
    loop
      delay Duration(args.period);
      msgs_num := msgs_num + 1;
      declare
        msg: string :=
          "Ada Server["& Trim(args.instance_id'Image, Ada.Strings.Left) &"] "
          & "msg #"& Trim(msgs_num'Image, Ada.Strings.Left) &";;;;"
          & "Ada Server["& Trim(args.instance_id'Image, Ada.Strings.Left) &"] "
          & UDP.INADDR_ANY & " => "& Trim(args.broadcast_host, Ada.Strings.Right) &":"
          & Trim(args.broadcast_port'Image, Ada.Strings.Left) &";;;;";
        msg_len: Positive := msg'Length;
        bytes_sent: Integer := 0;
      begin
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
  Put_Line("Server_Ada");
  New_Line;
  Put_Line("Instance:       " & Trim(args.instance_id'Image, Ada.Strings.Left));
  Put_Line("Broadcast Host: " & Trim(args.broadcast_host, Ada.Strings.Left));
  Put_Line("Broadcast Port: " & Trim(args.broadcast_port'Image, Ada.Strings.Left));
  Put_Line("Period:         " & Trim(args.period'Image, Ada.Strings.Left));
  --  Initialize Socket
  sockfd := UDP.get_broadcast_socket;
  if sockfd < 0 then
    Put_Line("Error: Could not acquire socket.");
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
  null;
end Server_Ada;
