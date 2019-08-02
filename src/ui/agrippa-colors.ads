package Agrippa.Colors is

   type Color_Element is mod 256;

   type Agrippa_Color is
      record
         Red, Green, Blue, Alpha : Color_Element;
      end record;

   function To_Html_Color_String
     (Color : Agrippa_Color)
      return String;

end Agrippa.Colors;
