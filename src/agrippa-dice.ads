private with WL.Random;

package Agrippa.Dice is

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
