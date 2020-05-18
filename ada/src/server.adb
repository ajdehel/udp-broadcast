with Ada.Text_IO;             use Ada.Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Bounded_Synchronized_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with UDP;

package body Server is

  ------------------------------------------------------------------------------
  --  Queue
  ------------------------------------------------------------------------------
  package Server_Queue_Interface is
    new Ada.Containers.Synchronized_Queue_Interfaces(Unbounded_String);
  package Unbounded_Server_Queues is
    new Ada.Containers.Unbounded_Synchronized_Queues(Server_Queue_Interface);
  subtype Unbounded_Server_Queue is Unbounded_Server_Queues.Queue;
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  --  Serve_Task_Type
  ------------------------------------------------------------------------------
  task type Serve_Task_Type is
    entry Initialize(id:    Natural;
                     host:  Unbounded_String;
                     port:  Natural);
    entry Start;
    entry Stop;
  end Serve_Task_Type;
  type Serve_Task_Access is access all Serve_Task_Type;

  package Serve_Task_Vectors is
    new Ada.Containers.Vectors(Natural, serve_task_access);
  subtype Serve_Task_Vector is Serve_Task_Vectors.Vector;
  ------------------------------------------------------------------------------


  Msg_Queue:   Unbounded_Server_Queue;
  Task_Vector: Serve_Task_Vector;


  ------------------------------------------------------------------------------
  --  Procedures
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  procedure Initialize(host: in String;
                       port: in Natural;
                       num_threads: in Positive)
  is
    size: Ada.Containers.Count_Type;
  begin
    size := Ada.Containers.Count_Type(num_threads);
    Task_Vector := Serve_Task_Vectors.To_Vector(size);
    for i_task in Task_Vector.first_index..Task_Vector.last_index loop
      Task_Vector(i_task) := new Serve_Task_Type;
      Task_Vector(i_task).Initialize(i_task, To_Unbounded_String(host), port);
    end loop;
  end Initialize;

  ------------------------------------------------------------------------------
  procedure Start
  is
  begin
    for i_task in Task_Vector.first_index..Task_Vector.last_index loop
      Task_Vector(i_task).Start;
    end loop;
  end Start;

  ------------------------------------------------------------------------------
  procedure Stop
  is
  begin
    for i_task in Task_Vector.first_index..Task_Vector.last_index loop
      Task_Vector(i_task).Stop;
    end loop;
  end Stop;

  ------------------------------------------------------------------------------
  procedure Send(message: in String)
  is
  begin
    Unbounded_Server_Queues.Enqueue(Msg_Queue, To_Unbounded_String(message));
  end Send;

  ------------------------------------------------------------
  --  Serve_Task_Type body
  ------------------------------------------------------------
  task body serve_task_type is
    task_id:        Natural;
    broadcast_host: Unbounded_String;
    broadcast_port: Natural;
    socket:         Integer;
  begin
    accept Initialize(id: Natural;
                      host: Unbounded_String;
                      port: Natural) do
      task_id := id;
      broadcast_host := host;
      broadcast_port := port;
    end Initialize;
    socket := UDP.get_broadcast_socket;
    accept Start;
    loop
      declare
        msg: Unbounded_String;
        bytes_sent: Integer;
        task_str: String := "Ada Server Task["& Trim(task_id'Image, Ada.Strings.Left) &"];;;;";
      begin
        select
          accept Stop;
        else
          Unbounded_Server_Queues.Dequeue(Msg_Queue, msg);
          Append(msg, task_str);
          Put_Line(task_id'Image & ":" & To_String(msg));
          bytes_sent := UDP.broadcast_msg(socket,
                                          To_String(broadcast_host),
                                          broadcast_port,
                                          To_String(msg),
                                          Length(msg));
        end select;
      end;
    end loop;
  end serve_task_type;
  ------------------------------------------------------------

end Server;

