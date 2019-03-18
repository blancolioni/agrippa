with Agrippa.State;

package Agrippa.Phases is

   type Phase_State_Type is
     new Agrippa.State.End_State_Interface with private;

   function Is_Finished
     (Phase_State : Phase_State_Type'Class)
      return Boolean;

   type Phase_Interface is interface;

   function Name
     (Phase : Phase_Interface)
      return String
      is abstract;

   function Start
     (Phase : Phase_Interface;
      State : in out Agrippa.State.State_Interface'Class)
     return Phase_State_Type'Class
   is abstract;

   function Current_Step_Name
     (Phase       : Phase_Interface;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String
      is abstract;

   procedure Step
     (Phase       : Phase_Interface;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class)
   is abstract;

private

   type Phase_State_Type is
     new Agrippa.State.End_State_Interface with
      record
         Finished    : Boolean := False;
         End_Of_Game : Boolean := False;
         Has_Winner  : Boolean := False;
         Winner      : Faction_Id := Faction_Id'First;
      end record;

   overriding function End_Of_Game
     (Phase_State : Phase_State_Type)
      return Boolean
   is (Phase_State.End_Of_Game);

   overriding function Has_Winner
     (Phase_State : Phase_State_Type)
      return Boolean
   is (Phase_State.Has_Winner);

   overriding function Winning_Faction
     (Phase_State : Phase_State_Type)
      return Faction_Id
   is (Phase_State.Winner);

   function Is_Finished
     (Phase_State : Phase_State_Type'Class)
      return Boolean
   is (Phase_State.Finished);

end Agrippa.Phases;
