with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Vectors;

procedure December_13 is

   subtype Time_Stamps is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Time_IO is new Ada.Text_IO.Integer_IO (Time_Stamps);

   subtype Bus_IDs is Long_Long_Integer range 1 .. Long_Long_Integer'Last;

   type Bus_List_Elements is record
      Bus_ID : Bus_IDs;
      Offset : Time_Stamps;
   end record;

   package Bus_Lists is new
     Ada.Containers.Vectors (Positive, Bus_List_Elements);
   use Bus_Lists;

   function "<" (Left, Right : Bus_List_Elements) return Boolean is

   begin -- "<"
      return Left.Bus_ID < Right.Bus_ID;
   end "<";

   package Bus_ID_Sort is new Bus_Lists.Generic_Sorting;

   Delimiter : constant Character_Set := To_Set (',');
   Input_File : File_Type;
   Text : Unbounded_String;
   Time_Stamp : Time_Stamps;
   Bus_List_Element : Bus_List_Elements;
   Bus_List : Bus_Lists.Vector := Empty_Vector;
   Start_At, First : Positive;
   Last : Natural;
   Time_To_Next_Bus : Time_Stamps := Time_Stamps'Last;
   Next_Bus : Bus_IDs;

begin -- December_13
   Open (Input_File, In_File, "december_13.txt");
   -- Open (Input_File, In_File, "example_13.txt");
   Time_IO.Get (Input_File, Time_Stamp);
   Skip_Line (Input_File);
   Get_Line (Input_File, Text);
   Close (Input_File);
   Start_At := 1;
   Bus_List_Element.Offset := 0;
   while Start_At <= Length (Text) loop
      Find_Token (Text, Delimiter, Start_At, Outside, First, Last);
      if Slice (Text, First, Last) /= "x" then
         Bus_List_Element.Bus_ID := Bus_IDs'Value ( Slice (Text, First, Last));
         Append (Bus_List, Bus_List_Element);
      end if; -- Slice (Text, First, Last) /= "x"
      Start_At := Last + 1;
      Bus_List_Element.Offset := Bus_List_Element.Offset + 1;
   end loop; -- Start_At < Length (Text)
   for B in Iterate (Bus_List) loop
      if Bus_List (B).Bus_ID - (Time_Stamp mod Bus_List (B).Bus_ID) <
        Time_To_Next_Bus then
         Time_To_Next_Bus := Bus_List (B).Bus_ID -
           (Time_Stamp mod Bus_List (B).Bus_ID);
         Next_Bus := Bus_List (B).Bus_ID;
      end if; -- Bus_List (B).Bus_ID - (Time_Stamp mod Bus_List (B).Bus_ID) ...
   end loop; -- B in Iterate (Bus_List)
   Put_Line ("BusID * wait time (Part one):" &
               Time_Stamps'Image (Time_Stamps (Next_Bus) * Time_To_Next_Bus));
   declare
      N : Time_Stamps := 1; -- number of runs of bus with largest Bus_ID
      B : Bus_Lists.Cursor;
      Found : Boolean;
   begin -- Part two
      Bus_ID_Sort.Sort (Bus_List); -- sorted smallest to largest Bus_ID
      while N * Last_Element (Bus_List).Bus_ID <
        Last_Element (Bus_List).Offset loop
         N := N + 1;
      end loop; --  N * Last_Element (Bus_List).Bus_ID < ...
      -- Ensures that the first Time_Stamp is 0 or more
      loop -- N
         B := Bus_Lists.Last (Bus_List);
         Time_Stamp := N * Bus_List (B).Bus_ID - Bus_List (B).Offset;
         Found := True;
         Previous (B);
         while B /= No_Element and Found loop
            Found :=
              (Time_Stamp + Bus_List (B).Offset) mod Bus_List (B).Bus_ID = 0;
            Previous (B);
         end loop; -- B /= No_Element and then ...
         exit when Found;
         N := N + 1;
      end loop; -- N
      Put_Line ("Time_Stamp (Part two)" & Time_Stamps'Image (Time_Stamp));
   end; -- Part two
end December_13;
