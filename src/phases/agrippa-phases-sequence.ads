package Agrippa.Phases.Sequence is

   type Phase_Id is private;

   function Show (Id : Phase_Id) return String;

   function First_Phase return Phase_Id;
   function Is_Last (Id : Phase_Id) return Boolean;
   function Next_Phase (Current : Phase_Id) return Phase_Id
     with Pre => not Is_Last (Current);

   function Phase (Id : Phase_Id) return Phase_Interface'Class;

private

   type Phase_Id is range 1 .. 7;

   function First_Phase return Phase_Id is (Phase_Id'First);

   function Is_Last (Id : Phase_Id) return Boolean
   is (Id = Phase_Id'Last);

   function Next_Phase (Current : Phase_Id) return Phase_Id
   is (Current + 1);

   function Show (Id : Phase_Id) return String
   is (case Id is
          when 1 => "I",
          when 2 => "II",
          when 3 => "III",
          when 4 => "IV",
          when 5 => "V",
          when 6 => "VI",
          when 7 => "VII");

end Agrippa.Phases.Sequence;
