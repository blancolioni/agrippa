private with Ada.Containers.Doubly_Linked_Lists;

with Agrippa.State;

package Agrippa.Models is

   type Model_Watcher is limited interface;

   procedure Model_Changed
     (Watcher : in out Model_Watcher)
   is abstract;

   type Root_Agrippa_Model is abstract tagged private;

   function Title
     (Model : Root_Agrippa_Model)
      return String
      is abstract;

   procedure Update
     (Model : in out Root_Agrippa_Model)
   is null;

   procedure Add_Watcher
     (Model   : in out Root_Agrippa_Model'Class;
      Watcher : not null access Model_Watcher'Class);

   function State
     (Model : Root_Agrippa_Model'Class)
      return access constant Agrippa.State.State_Interface'Class;

   procedure Set_State
     (Model : in out Root_Agrippa_Model'Class;
      State : not null access Agrippa.State.State_Interface'Class);

   procedure Notify_Changed
     (Model : in out Root_Agrippa_Model);

   type Model_Type is access all Root_Agrippa_Model'Class;

private

   type Model_Watcher_Access is
     access all Model_Watcher'Class;

   package Model_Watcher_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Model_Watcher_Access);

   type Root_Agrippa_Model is abstract tagged
      record
         Watchers : Model_Watcher_Lists.List;
         State    : access Agrippa.State.State_Interface'Class;
      end record;

   function State
     (Model : Root_Agrippa_Model'Class)
      return access constant Agrippa.State.State_Interface'Class
   is (Model.State);

end Agrippa.Models;
