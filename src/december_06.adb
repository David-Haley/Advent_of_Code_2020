with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;

procedure December_06 is

   subtype Questions is Character range 'a' .. 'z';

   package Question_Sets is new Ada.Containers.Ordered_Sets (Questions);
   use Question_Sets;

   procedure Set_All (Question_Set : out Question_Sets. Set) is

   begin -- Set_All
      for Q in Questions loop
         Include (Question_Set, Q);
      end loop; -- Q in Questions
   end Set_All;

   Input_File : File_Type;
   Text : Unbounded_String;
   Question_Set, Line_Set : Question_Sets.Set := Empty_Set;
   Question_Count : Count_Type := 0;

begin -- December_06
   Open (Input_File, In_File, "december_06.txt");
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Text);
      for I in Positive range 1 .. Length (Text) loop
         Include (Question_Set, Element (Text, I));
      end loop; -- I in Positive range 1 .. Length (Text)
      if Length (Text) = 0  or End_Of_File (Input_File) then
         Question_Count := Question_Count + Length (Question_Set);
         Clear (Question_Set);
      end if; -- Length (Text) = 0  or End_Of_File (Input_File)
   end loop; -- not End_Of_File (Input_File)
   Put_Line ("Part One Sum:" & Count_Type'Image (Question_Count));
   Reset (Input_File);
   Question_Count := 0;
   Set_All (Question_Set);
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Text);
      if Length (Text) /= 0 then
         Clear (Line_Set);
         for I in Positive range 1 .. Length (Text) loop
            Include (Line_Set, Element (Text, I));
         end loop; -- I in Positive range 1 .. Length (Text)
         Question_Set := Intersection (Question_Set, Line_Set);
      end if; -- Length (Text) /= 0
      if Length (Text) = 0  or End_Of_File (Input_File) then
         Question_Count := Question_Count + Length (Question_Set);
         Set_All (Question_Set);
      end if; -- Length (Text) = 0  or End_Of_File (Input_File)
   end loop; -- not End_Of_File (Input_File)
   Put_Line ("Part Two Sum:" & Count_Type'Image (Question_Count));
   Close (Input_File);
end December_06;
