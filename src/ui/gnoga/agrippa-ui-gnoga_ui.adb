with Ada.Text_IO;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;

with Agrippa.Sessions;

package body Agrippa.UI.Gnoga_UI is

   procedure On_Connect_Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   ------------------------
   -- On_Connect_Default --
   ------------------------

   procedure On_Connect_Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      Session : constant Agrippa.Sessions.Agrippa_Session :=
                  Agrippa.Sessions.New_Session
                    (Main_Window'Unchecked_Access);
   begin
      Main_Window.Connection_Data (Session);

      Ada.Text_IO.Put_Line ("connected");
   end On_Connect_Default;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (UI  : in out Gnoga_UI_Type)
   is
      pragma Unreferenced (UI);
   begin
      Gnoga.Application.Title ("Agrippa");

      Gnoga.Application.HTML_On_Close ("Application disconnected.");

      Gnoga.Application.Multi_Connect.Initialize
        (Port    => 8080,
         Boot    => "agrippa.html",
         Verbose => True);

      Gnoga.Application.Multi_Connect.On_Connect_Handler
        (Event => Agrippa.UI.Gnoga_UI.On_Connect_Default'Unrestricted_Access,
         Path  => "default");

      Gnoga.Application.Multi_Connect.Message_Loop;
   end Start;

   -----------------
   -- Stop_Server --
   -----------------

   procedure Stop_Server (Message : String) is
   begin
      null;
   end Stop_Server;

end Agrippa.UI.Gnoga_UI;
