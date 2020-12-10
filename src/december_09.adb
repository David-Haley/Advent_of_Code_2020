with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_09 is

   Preamble : constant Positive := 25;

   subtype Indices is Positive;
   subtype Message_Elements is Long_Long_Integer;
   package XMAS_Messages is new
     Ada.Containers.Vectors (Indices, Message_Elements);
   use XMAS_Messages;

   package Message_IO is new Ada.Text_IO.Integer_IO (Message_Elements);
   use Message_IO;

   function Check_Sum (XMAS_Message : in XMAS_Messages.Vector;
                       To_Check : in Indices) return Boolean is

      First : constant Indices := To_Check - Preamble;
      Last : constant indices := To_Check - 1;
      Result : Boolean := False;

   begin -- Check_Sum
      for I in Indices range First .. Last loop
         for J in Indices range I + 1 .. Last loop
            Result := Result or (Element (XMAS_Message, I) /=
                                   Element (XMAS_Message, J) and then
                                  XMAS_Message (I) + XMAS_Message (J) =
                                   XMAS_Message (To_Check));
         end loop; -- J in Indices range I + 1 .. Last
      end loop; -- I in Indices range First .. Last
      return Result;
   end Check_Sum;

   function Pair_Sum (XMAS_Message : in XMAS_Messages.Vector;
                      To_Find : in Message_Elements) return Message_Elements is

      Start_At : Indices := 1;
      I : Indices;
      Sum : Message_Elements;
      Lowest : Message_Elements := Message_Elements'Last;
      Highest : Message_Elements := Message_Elements'First;

   begin -- Pair_Sum
      loop -- Start_At
         Sum := XMAS_Message (Start_At);
         I := Start_At + 1;
         loop -- I
            Sum := Sum + XMAS_Message (I);
            exit when Sum >= To_Find or I >= Last_Index (XMAS_Message);
            I := I + 1;
         end loop; -- I
         exit when Sum = To_Find or Start_At >= Last_Index (XMAS_Message);
         Start_At := Start_At + 1;
      end loop; -- Start_At
      if Sum = To_Find then
         for J in Indices range Start_At .. I loop
            if XMAS_Message (J) < Lowest then
               Lowest := XMAS_Message (J);
            end if; -- XMAS_Message (J) < Lowest
            if XMAS_Message (J) > Highest then
               Highest := XMAS_Message (J);
            end if; -- XMAS_Message (J) > Highest
         end loop; -- J in Indices range Start_At to I
         return Lowest + Highest;
      else
         return 1;
      end if; -- Sum = To_Find
   end Pair_Sum;

   Input_File : File_Type;
   XMAS_Message : XMAS_Messages.Vector := Empty_Vector;
   Message_Element, Invalid_Element, Sum : Message_Elements;
   To_Check : Indices := Preamble + 1;

begin -- December_09
   Open (Input_File, In_File, "december_09.txt");
   while not End_Of_File (Input_File) loop
      Get (Input_File, Message_Element);
      Skip_Line (Input_File);
      Append (XMAS_Message, Message_Element);
   end loop; -- not End_Of_File (Input_File)
   Close (Input_File );
   loop -- check one Message element
      exit when not Check_Sum (XMAS_Message, To_Check) or
        To_Check >= Last_Index (XMAS_Message);
      To_Check := To_Check + 1;
   end loop; -- check one Message element
   if not Check_Sum (XMAS_Message, To_Check) then
      Invalid_Element := XMAS_Message(To_Check);
      Put_Line ("Part one:" & Message_Elements'Image (Invalid_Element));
      Sum := Pair_Sum (XMAS_Message, Invalid_Element);
      if Sum = 1 then
         Put_Line ("Part two not found");
      else
         Put_Line ("Part two:" & Message_Elements'Image (Sum));
      end if; -- Sum = 1
   else
      Put_Line ("Part one not found");
   end if; --not Check_Sum (XMAS_Message, To_Check)
end December_09;
