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
with Server;

procedure Server_Ada is

  ----------------------------------------
  --  Tasks
  ----------------------------------------
  task Main_Loop is
    entry Start;
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
      broadcast_host: Unbounded_String;
      broadcast_port: Natural;
      instance_id:    Natural;
      period:         Float := 1.0;
      num_threads:    Positive := 1;
    end record;

  function Parse_Args return Arguments is
    parsed_args: Arguments;
    tmp_string: String(1..20);
  begin
    loop
      case GetOpt("p: t:") is
        when 'p' =>
          parsed_args.period := Float'Value(Parameter);
        when 't' =>
          parsed_args.num_threads := Positive'Value(Parameter);
        when others =>
          exit;
      end case;
    end loop;
    UDP.Split_IP( Get_Argument, tmp_string, parsed_args.broadcast_port );
    parsed_args.broadcast_host := To_Unbounded_String(tmp_string);
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
    msgs_num : Natural := 0;
  begin
    accept Start do
      Server.Start;
    end Start;
    loop
      delay Duration(args.period);
      msgs_num := msgs_num + 1;
      Put_Line(msgs_num'Image);
      declare
        msg: String :=
          "Ada Server["& Trim(args.instance_id'Image, Ada.Strings.Left) &"] "
          & "msg #"& Trim(msgs_num'Image, Ada.Strings.Left) &";;;;"
          & "Ada Server["& Trim(args.instance_id'Image, Ada.Strings.Left) &"] "
          & UDP.INADDR_ANY & " => "& To_String(Trim(args.broadcast_host, Ada.Strings.Right)) &":"
          & Trim(args.broadcast_port'Image, Ada.Strings.Left) &";;;;";
        bytes_sent: Integer := 0;
      begin
        Server.Send(msg);
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
begin
  Put_Line("Server_Ada");
  New_Line;
  Put_Line("Instance:       " & Trim(args.instance_id'Image, Ada.Strings.Left));
  Put_Line("Broadcast Host: " & Trim(To_String(args.broadcast_host), Ada.Strings.Left));
  Put_Line("Broadcast Port: " & Trim(args.broadcast_port'Image, Ada.Strings.Left));
  Put_Line("Period:         " & Trim(args.period'Image, Ada.Strings.Left));
  Signal_Handler.Start;
  Server.Initialize(To_String(args.broadcast_host), args.broadcast_port, args.num_threads);
  Main_Loop.Start;
  Program_Status.Wait;
  abort Main_Loop;
  abort Signal_Handler;
  Server.Stop;
end Server_Ada;

