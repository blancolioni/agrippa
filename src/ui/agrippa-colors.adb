package body Agrippa.Colors is

   ---------------------------
   -- To_Html_Color_String --
   ---------------------------

   function To_Html_Color_String
     (Color : Agrippa_Color)
      return String
   is
      function Hex_Digit (X : Color_Element) return Character
      is (if X < 10
          then Character'Val (X + 48)
          else Character'Val (X + 55));

      function Hex (X : Color_Element) return String
      is ((Hex_Digit (X / 16), Hex_Digit (X mod 16)));

   begin
      return "#"
        & Hex (Color.Red)
        & Hex (Color.Green)
        & Hex (Color.Blue);
   end To_Html_Color_String;

end Agrippa.Colors;
