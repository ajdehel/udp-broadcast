with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Interrupts;
with Ada.Interrupts.Names;

use Ada.Interrupts;
use Ada.Interrupts.Names;

pragma Interrupt_State(SIGINT, USER);

package body Signal_Handling is

  ------------------------------------------------------------------------------
  protected Handler is
    procedure Handle;
    pragma Interrupt_Handler(Handle);
    entry Wait;
    private
    Signal_Caught: Boolean := False;
  end Handler;

  ----------------------------------------------------------------------------
  protected body Handler is
    ------------------------------------------------------------------------
    procedure Handle is
    begin
      Signal_Caught := True;
    end Handle;
    ------------------------------------------------------------------------
    entry Wait when Signal_Caught = True is
    begin
      Signal_Caught := False;
    end Wait;
  end Handler;

  ----------------------------------------------------------------------------
  procedure Add_SIGINT is
  begin
    Attach_Handler(Handler.Handle'Access, SIGINT);
  end Add_SIGINT;

  ----------------------------------------------------------------------------
  procedure Wait is
  begin
    Handler.Wait;
  end Wait;

end Signal_Handling;
