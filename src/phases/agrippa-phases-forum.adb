with Ada.Containers.Vectors;

with Agrippa.Images;

with Agrippa.Messages;

package body Agrippa.Phases.Forum is

   type Forum_Step is
     (Initiative, Curia);

   type Initiative_Index is range 1 .. 6;

   type Initiative_Action is
     (Initiative_Roll, Persuasion, Knights);

   package Initiative_Order_Vectors is
     new Ada.Containers.Vectors (Initiative_Index, Faction_Id);

   type Forum_Phase_State is
     new Phase_State_Type with
      record
         Step               : Forum_Step := Initiative;
         Current_Initiative : Initiative_Index := 1;
         Current_Action     : Initiative_Action := Initiative_Roll;
         Initiative_Order   : Initiative_Order_Vectors.Vector;
      end record;

   type Forum_Phase_Type is
     new Phase_Interface with null record;

   overriding function Name
     (Phase : Forum_Phase_Type)
      return String;

   overriding function Start
     (Phase : Forum_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class;

   overriding function Current_Step_Name
     (Phase       : Forum_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String;

   overriding procedure Step
     (Phase       : Forum_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class);

   -----------------------
   -- Current_Step_Name --
   -----------------------

   overriding function Current_Step_Name
     (Phase       : Forum_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String
   is
      Forum_State : Forum_Phase_State renames
                        Forum_Phase_State (Phase_State);
      pragma Unreferenced (Phase);
   begin
      case Forum_State.Step is
         when Initiative =>
            if Forum_State.Current_Action = Initiative_Action'First then
               return Agrippa.Images.Sequence_Image
                 (Positive (Forum_State.Current_Initiative))
                 & " Initiative: "
                 & State.Faction_Name (Forum_State.Initiative_Order.Element
                                       (Forum_State.Current_Initiative));
            else
               return "";
            end if;
         when Curia =>
            return "Curia Rolls";
      end case;
   end Current_Step_Name;

   -----------------
   -- Forum_Phase --
   -----------------

   function Forum_Phase return Phase_Interface'Class is
   begin
      return Phase : Forum_Phase_Type;
   end Forum_Phase;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Phase : Forum_Phase_Type)
      return String
   is
      pragma Unreferenced (Phase);
   begin
      return "forum-phase";
   end Name;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Phase : Forum_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class
   is
      pragma Unreferenced (Phase);
   begin
      State.Clear_Status;
      return Phase_State : Forum_Phase_State do
         declare
            Next : Faction_Id :=
                     State.Senator_Faction
                       (State.Highest_Ranking_Available_Officer);
         begin
            for Faction in Faction_Id loop
               Phase_State.Initiative_Order.Append (Next);
               if Next = Faction_Id'Last then
                  Next := Faction_Id'First;
               else
                  Next := Faction_Id'Succ (Next);
               end if;
            end loop;
         end;
      end return;
   end Start;

   ----------
   -- Step --
   ----------

   overriding procedure Step
     (Phase       : Forum_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class)
   is
      pragma Unreferenced (Phase);
      Forum_State : Forum_Phase_State renames
                      Forum_Phase_State (Phase_State);

   begin
      case Forum_State.Step is
         when Initiative =>
            declare
               Faction : constant Faction_Id :=
                           Forum_State.Initiative_Order.Element
                             (Forum_State.Current_Initiative);
            begin
               case Forum_State.Current_Action is
                  when Initiative_Roll =>
                     State.Send_Message
                       (Agrippa.Messages.Initiative_Roll (Faction));
--
--                       State.Execute_Initiative_Roll (Faction);
                  when Persuasion =>
                     null;
                  when Knights =>
                     State.Send_Message
                       (Agrippa.Messages.Attract_Knights (Faction));
               end case;

               if Forum_State.Current_Action = Initiative_Action'Last then
                  if Forum_State.Current_Initiative
                    = Initiative_Index'Last
                  then
                     Forum_State.Step := Curia;
                  else
                     Forum_State.Current_Initiative :=
                       Forum_State.Current_Initiative + 1;
                     Forum_State.Current_Action := Initiative_Roll;
                  end if;
               else
                  Forum_State.Current_Action :=
                    Initiative_Action'Succ (Forum_State.Current_Action);
               end if;
            end;
         when Curia =>
            Forum_State.Finished := True;
      end case;

   end Step;

end Agrippa.Phases.Forum;
