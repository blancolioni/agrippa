private with Ada.Strings.Fixed;

package Agrippa.Images is

   function Image (Cash : Talents) return String;
   function Image (Attribute : Attribute_Range) return String;
   function Image (Influence : Influence_Range) return String;
   function Image (Popularity : Popularity_Range) return String;
   function Image (Votes : Vote_Count) return String;
   function Image (Count : Military_Unit_Count) return String;
   function Image (Value : Integer) return String;

   function Image (Value    : Integer;
                   Singular : String;
                   Plural   : String := "")
                   return String;
   function Sequence_Image
     (Index : Positive)
      return String;

   function Trim (Img : String) return String;

private

   function Trim (Img : String) return String
   is (Ada.Strings.Fixed.Trim (Img, Ada.Strings.Both));

   function Image (Attribute : Attribute_Range) return String
   is (Trim (Attribute'Image));

   function Image (Influence : Influence_Range) return String
   is (Trim (Influence'Image));

   function Image (Popularity : Popularity_Range) return String
   is (Trim (Popularity'Image));

   function Image (Votes : Vote_Count) return String
   is (Trim (Votes'Image));

   function Image (Cash : Talents) return String
   is (Trim (Cash'Image));

   function Image (Count : Military_Unit_Count) return String
   is (Trim (Count'Image));

   function Image (Value : Integer) return String
   is (Trim (Value'Image));

   function Image (Value    : Integer;
                   Singular : String;
                   Plural   : String := "")
                   return String
   is (Trim (Value'Image) & " "
       & (if Value = 1 then Singular
          elsif Plural = "" then Singular & "s"
          else Plural));

   function Sequence_Image
     (Index : Positive)
      return String
   is (Trim (Index'Image)
       & (if Index mod 10 = 1 and then Index /= 11
          then "st"
          elsif Index mod 10 = 2 and then Index /= 12
          then "nd"
          elsif Index mod 10 = 3 and then Index /= 13
          then "rd"
          else "th"));

end Agrippa.Images;
