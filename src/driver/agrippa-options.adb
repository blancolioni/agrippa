with WL.Command_Line;

package body Agrippa.Options is

   pragma Style_Checks (Off);

   function Randomise return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("randomise", ' ');
   end Randomise;

   function Random_Seed return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("random-seed", ' ', 0);
   end Random_Seed;

   function Gnoga_UI return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("gnoga-ui", ' ');
   end Gnoga_UI;

   function Text_UI return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("text-ui", ' ');
   end Text_UI;

end Agrippa.Options;
