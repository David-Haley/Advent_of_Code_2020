with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_02 is

   Input_File : File_Type;
   subtype Occurances is Positive;
   subtype Letters is Character range 'a' .. 'z';

   package Occurance_IO is new Ada.Text_IO.Integer_IO (Occurances);
   use Occurance_IO;
   Text : Unbounded_String;

   Ch : Character;
   Least, Most, First, Second : Occurances;
   Repeated_Letter, Letter : Letters;
   Occurance_Count : Natural;
   Valid_Count : Natural := 0;

begin -- December_01
   Open (Input_File, In_File, "december_02.txt");
   while not End_Of_File (Input_File) loop
      Get (Input_File, Least);
      Get (Input_File, Ch);
      Assert (Ch = '-', "Expected '-' found '" & Ch & "'");
      Get (Input_File, Most);
      Get (Input_File, Ch);
      Assert (Ch = ' ', "Expected ' ' found '" & Ch & "'");
      Get (Input_File, Repeated_Letter);
      Get (Input_File, Ch);
      Assert (Ch = ':', "Expected ':' found '" & Ch & "'");
      Get (Input_File, Ch);
      Assert (Ch = ' ', "Expected ' ' after ':' found '" & Ch & "'");
      Occurance_Count := 0;
      while not End_Of_Line (Input_File) loop
         Get (Input_File, Letter);
         if Letter = Repeated_Letter then
            Occurance_Count := Occurance_Count + 1;
         end if; -- Letter = Repeated_Letter
      end loop; -- not End_Of_Line (Input_File)
      if Occurance_Count >= Least and Occurance_Count <= Most then
         Valid_Count := Valid_Count + 1;
      end if; -- Occurance_Counte >= Least and Occurance_Count <= Most)
   end loop; -- End_Of_File (Input_File)
   Put_Line ("Valid Passwords:" & Natural'Image (Valid_Count));
   Put_Line ("Part Two");
   Valid_Count := 0;
   reset (Input_File);
   -- Line formatting checks are mot repeated as the second read will not occur
   -- if any assertion fails in the first read;
   while not End_Of_File (Input_File) loop
      Get (Input_File, First);
      Get (Input_File, Ch);
      Get (Input_File, Second);
      Get (Input_File, Ch);
      Get (Input_File, Repeated_Letter);
      Get (Input_File, Ch);
      Get (Input_File, Ch);
      Text := Get_line (Input_File);
      if (First <= Length (Text) and Second <= length (Text)) and then
        (Repeated_Letter = Element (Text, First) xor
             Repeated_Letter = Element (Text, Second)) then
              Valid_Count := Valid_Count + 1;
      end if; -- (First < Length (text) and Second (Text)) and then ...
   end loop; -- not End_Of_File (Input_File)
   Put_Line ("Valid Passwords:" & Natural'Image (Valid_Count));
   Close (Input_File);
end December_02;
