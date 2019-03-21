with Agrippa.Cards.Wars;

package Agrippa.State.Wars is

   type War_State_Type is
     new War_State_Interface with private;

   function Tag
     (State : War_State_Type'Class)
      return String;

   overriding function Drawn
     (War : War_State_Type)
      return Boolean;

   procedure Initialize
     (War     : in out War_State_Type'Class;
      Id      : War_Id;
      In_Play : Boolean);

   procedure On_Drawn
     (War : in out War_State_Type);

   procedure Activate
     (War : in out War_State_Type);

   procedure Prosecute
     (War : in out War_State_Type)
     with Pre => War.Drawn;

   procedure Discard
     (War : in out War_State_Type)
     with Pre => War.Drawn;

private

   type War_State_Type is
     new War_State_Interface with
      record
         Id             : War_Id;
         In_Play        : Boolean := False;
         Imminent       : Boolean := False;
         Active         : Boolean := False;
         Drawn          : Boolean := False;
         Discarded      : Boolean := False;
         Prosecuted     : Boolean := False;
      end record;

   overriding function Id
     (State : War_State_Type)
      return War_Id
   is (State.Id);

   overriding function In_Play
     (War : War_State_Type)
      return Boolean
   is (War.In_Play);

   overriding function Imminent
     (War : War_State_Type)
      return Boolean
   is (War.Imminent);

   overriding function Active
     (War : War_State_Type)
      return Boolean
   is (War.Active);

   overriding function Inactive
     (War : War_State_Type)
      return Boolean
   is (War.In_Play
       and then War.Drawn
       and then not War.Discarded
       and then not War.Active);

   overriding function Discarded
     (War : War_State_Type)
      return Boolean
   is (War.Discarded);

   overriding function Prosecuted
     (War : War_State_Type)
      return Boolean
   is (War.Prosecuted);

   overriding function Land_Strength
     (State : War_State_Type)
      return Legion_Count
   is (Agrippa.Cards.Wars.War (State.Id).Land_Strength);

   overriding function Fleet_Support
     (State : War_State_Type)
      return Fleet_Count
   is (Agrippa.Cards.Wars.War (State.Id).Fleet_Support);

   overriding function Fleet_Strength
     (State : War_State_Type)
      return Fleet_Count
   is (Agrippa.Cards.Wars.War (State.Id).Fleet_Strength);

   overriding function Drawn
     (War : War_State_Type)
      return Boolean
   is (War.Drawn);

end Agrippa.State.Wars;
