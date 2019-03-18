package Agrippa.Cards.Provinces is

   type Province_Card_Type is
     new Card_Type with private;

private

   type Province_Card_Type is
     new Card_Type with
      record
         null;
      end record;

   procedure New_Province
     (Province : in out Province_Card_Type'Class);

end Agrippa.Cards.Provinces;
