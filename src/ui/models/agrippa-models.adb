package body Agrippa.Models is

   -----------------
   -- Add_Watcher --
   -----------------

   procedure Add_Watcher
     (Model   : in out Root_Agrippa_Model'Class;
      Watcher : not null access Model_Watcher'Class)
   is
   begin
      Model.Watchers.Append (Model_Watcher_Access (Watcher));
   end Add_Watcher;

   --------------------
   -- Notify_Changed --
   --------------------

   procedure Notify_Changed
     (Model : in out Root_Agrippa_Model)
   is
   begin
      for Watcher of Model.Watchers loop
         Watcher.Model_Changed;
      end loop;
   end Notify_Changed;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
     (Model : in out Root_Agrippa_Model'Class;
      State : not null access Agrippa.State.State_Interface'Class)
   is
   begin
      Model.State := State;
   end Set_State;

end Agrippa.Models;
