with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Maps;
with Interfaces; use Interfaces;

procedure December_14 is

   subtype Mem_Words is Unsigned_64;
   MSB : constant Natural := 35;
   subtype Bit_Numbers is Natural range 0 .. MSB;

   Bit_35 : constant Mem_Words := 16#0000_0008_0000_0000#;
   Mask : constant String := "mask = ";
   Data : Mem_Words;

   package Memories is new Ada.Containers.Ordered_Maps (Mem_Words, Mem_Words);
   use Memories;

   package X_Locations is new
     Ada.Containers.Ordered_Maps (Bit_Numbers, Bit_Numbers);
   use X_Locations;

   Delimiter : constant Character_Set := To_Set (',');
   Input_File : File_Type;
   Text, Current_Mask: Unbounded_String;
   Start_At, First : Positive;
   Last : Natural;
   Memory : Memories.Map;
   Address : Mem_Words;
   Sum : Mem_Words;
   X_Location : X_Locations.Map;
   Fixed_Address : Mem_Words;
   X_Count : Natural;
   Permutations : Mem_Words;

begin -- December_14
   Open (Input_File, In_File, "december_14.txt");
   while not End_Of_Line (Input_File) loop
      Get_Line (Input_File, Text);
      if index (Text, Mask) /= 0 then
         Current_Mask := Delete (Text, 1, Mask'Length);
      elsif Index (Text, "mem") /= 0 then
         Find_Token (Text, Decimal_Digit_Set, Inside, First, Last);
         Address := Mem_Words'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Data := Mem_Words'Value (Slice (Text, First, Last));
         for I in Positive range 1 .. Length (Current_Mask) loop
            if Element (Current_Mask, I) = '0' then
               Data := Data and not Shift_Right (Bit_35, I - 1);
            elsif Element (Current_Mask, I) = '1' then
               Data := Data or Shift_Right (Bit_35, I - 1);
            else
               Assert (Element (Current_Mask, I) = 'X',
                       "Expected 'X' and found '" &
                         Element (Current_Mask, I) & "' at Line:" &
                         Positive_Count'Image (Line (Input_File) - 1));
            end if; -- Element (Current_Mask, I) = '0'
         end loop; -- I in Positive range 1 .. Length (Current_Mask)
         Include (Memory, Address, Data);
      end if; -- index (Text, Mask) /= 0
   end loop; -- not End_Of_Line (Input_File)
   sum := 0;
   for A in Iterate (Memory) loop
      Sum := Sum + Memory (A);
   end loop; -- A in Iterate (Memory)
   Put_Line ("Sum (Part one):" & Mem_Words'Image (Sum));
   -- Start of part two
   Reset (Input_File);
   Clear (Memory);
   while not End_Of_Line (Input_File) loop
      Get_Line (Input_File, Text);
      if index (Text, Mask) /= 0 then
         Current_Mask := Delete (Text, 1, Mask'Length);
         X_Count := 0;
         Clear (X_Location);
         for I in Positive range 1 .. Length (Current_Mask) loop
            if Element (Current_Mask, I) = 'X' then
               Include (X_Location, X_Count, MSB + 1 - I);
               X_Count := X_Count + 1;
            end if; -- Element (Current_Mask, I) = 'X'
         end loop; -- I in Positive range 1 .. Length (Current_Mask)
      elsif Index (Text, "mem") /= 0 then
         Find_Token (Text, Decimal_Digit_Set, Inside, First, Last);
         Fixed_Address := Mem_Words'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Data := Mem_Words'Value (Slice (Text, First, Last));
         for I in Positive range 1 .. Length (Current_Mask) loop
            if Element (Current_Mask, I) = 'X' then
               -- Clear any 1s in Fix_Address corrosponding to Xs
               Fixed_Address := Fixed_Address and not
                 Shift_Right (Bit_35, I - 1);
            elsif Element (Current_Mask, I) = '1' then
               Fixed_Address := Fixed_Address or Shift_Right (Bit_35, I - 1);
            else
               Assert (Element (Current_Mask, I) = '0',
                       "Expected '0' and found '" & Element (Current_Mask, I) &
                         "' at Line:" &
                         Positive_Count'Image (Line (Input_File) - 1));
            end if; -- Element (Current_Mask, I) = '0'
         end loop; -- I in Positive range 1 .. Length (Current_Mask)
         Permutations := 2 ** X_Count - 1;
         loop -- Permutations
            Address := 0;
            -- The for loop below may cause a runtime exception if there are no
            -- Xs, that is, Permutations is 0 and X_Count is 0, hence
            -- X_count - 1 will evaluate as -1 which in not in Natural!
            for I in Natural range 0 .. X_Count - 1 loop
               if (Shift_Left (1, I) and Permutations) /= 0 then
                  Address := Address or Shift_Left (1, X_Location (I));
               end if; -- I in Natural range 0 .. X_Count - 1
            end loop; -- I in Natural range 0 .. X_Count - 1
            Address := Address or Fixed_Address;
            Include (Memory, Address, Data);
            exit when Permutations = 0;
            Permutations := Permutations - 1;
         end loop; -- Permutations
      end if; -- index (Text, Mask) /= 0
   end loop; -- not End_Of_Line (Input_File)
   sum := 0;
   for A in Iterate (Memory) loop
      Sum := Sum + Memory (A);
   end loop; -- A in Iterate (Memory)
   Put_Line ("Sum (Part two):" & Mem_Words'Image (Sum));
   Close (Input_File);
end December_14;
