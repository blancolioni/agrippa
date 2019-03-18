private with WL.Random;

package Agrippa.Dice is

   subtype Die_Range is Integer range 1 .. 6;

   subtype DR_Range is Integer range 2 .. 12;

   subtype TDR_Range is Integer range 3 .. 18;

   function Roll_Die return Die_Range;

   function DR return DR_Range;
   function TDR return TDR_Range;

private

   function Roll_Die return Die_Range
   is (WL.Random.Random_Number (1, 6));

   function DR return DR_Range
   is (Roll_Die + Roll_Die);

   function TDR return TDR_Range
   is (Roll_Die + Roll_Die + Roll_Die);

end Agrippa.Dice;
