private with Ada.Containers.Indefinite_Vectors;
private with WL.String_Maps;

package Agrippa.Json is

   type Json_Value is abstract tagged private;

   function Null_Value return Json_Value'Class;
   function String_Value (Text : String) return Json_Value'Class;
   function Integer_Value (Int : Integer) return Json_Value'Class;

   function Serialize
     (Value : Json_Value)
      return String
      is abstract;

   type Json_Object is new Json_Value with private;

   overriding function Serialize
     (Value : Json_Object)
      return String;

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : Json_Value'Class);

   procedure Set_Property
     (Object : in out Json_Object'Class;
      Name   : String;
      Value  : String);

   type Json_Array is new Json_Value with private;

   overriding function Serialize
     (Value : Json_Array)
      return String;

   procedure Append
     (To     : in out Json_Array'Class;
      Value  : Json_Value'Class);

private

   type Json_Value is abstract tagged null record;

   package Json_Value_Maps is
     new WL.String_Maps (Json_Value'Class);

   package Json_Value_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Json_Value'Class);

   type Json_Object is new Json_Value with
      record
         Properties : Json_Value_Maps.Map;
      end record;

   type Json_Array is new Json_Value with
      record
         Vector : Json_Value_Vectors.Vector;
      end record;

end Agrippa.Json;
