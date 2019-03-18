with Ada.Containers.Indefinite_Vectors;

package Agrippa.Cards.Vectors is
  new Ada.Containers.Indefinite_Vectors (Positive, Card_Type'Class);
