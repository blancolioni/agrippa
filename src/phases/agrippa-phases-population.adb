with Agrippa.Messages;

package body Agrippa.Phases.Population is

   type Population_Phase_Type is
     new Phase_Interface with null record;

   overriding function Name
     (Phase : Population_Phase_Type)
      return String;

   overriding function Start
     (Phase : Population_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class;

   overriding function Current_Step_Name
     (Phase       : Population_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String;

   overriding procedure Step
     (Phase       : Population_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class);

   -----------------------
   -- Current_Step_Name --
   -----------------------

   overriding function Current_Step_Name
     (Phase       : Population_Phase_Type;
      Phase_State : Phase_State_Type'Class;
      State       : Agrippa.State.State_Interface'Class)
      return String
   is
      pragma Unreferenced (Phase, State, Phase_State);
   begin
      return "";
   end Current_Step_Name;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Phase : Population_Phase_Type)
      return String
   is
      pragma Unreferenced (Phase);
   begin
      return "population-phase";
   end Name;

   ----------------------
   -- Population_Phase --
   ----------------------

   function Population_Phase return Phase_Interface'Class is
   begin
      return Phase : Population_Phase_Type;
   end Population_Phase;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Phase : Population_Phase_Type;
      State : in out Agrippa.State.State_Interface'Class)
      return Phase_State_Type'Class
   is
      pragma Unreferenced (Phase, State);
   begin
      return Phase_State : Phase_State_Type;
   end Start;

   ----------
   -- Step --
   ----------

   overriding procedure Step
     (Phase       : Population_Phase_Type;
      Phase_State : in out Phase_State_Type'Class;
      State       : in out Agrippa.State.State_Interface'Class)
   is
      pragma Unreferenced (Phase);
   begin
      State.Send_Message (Agrippa.Messages.Population_Roll);
      Phase_State.Finished := True;
   end Step;

end Agrippa.Phases.Population;
