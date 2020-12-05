with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Sets;

procedure December_05 is

   subtype Rows is Natural range 0 .. 127;
   subtype Columns is Natural range 0 .. 7;
   Row_Multiplier : constant Natural := Columns'Last + 1;
   subtype Seats is Natural range 0 ..
     Rows'Last * Row_Multiplier + Columns'Last;

   package Seat_Sets is new Ada.Containers.Ordered_Sets (Seats);
   use Seat_Sets;

   Input_File : File_Type;
   Text : Unbounded_String;
   Highest_Seat : Seats := Seats'First;
   Lowest_Seat : Seats := Seats'Last;
   Row : Rows;
   Column : Columns;
   Multiplier : Natural;
   Seat_Set : Seat_Sets.Set := Empty_Set;
   This_Seat : Seats;

begin -- December_05
   Open (Input_File, In_File, "december_05.txt");
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Text);
      Row := 0;
      Multiplier := (Rows'Last + 1) / 2;
      for I in Positive range 1 .. 7 loop
         if Element (Text, I) = 'B' then
            Row := Row + Multiplier;
         else
            Assert (Element (Text, I) = 'F', "Expected 'F' and found '" &
                      Element (Text, I) & "'");
         end if; -- Element (I) = 'F'
         Multiplier := Multiplier / 2;
      end loop; -- I in Positive range 1 .. 7
      Column := 0;
      Multiplier := (Columns'Last + 1) / 2;
      for I in Positive range 8 .. 10 loop
         if Element (Text, I) = 'R' then
            Column := Column + Multiplier;
         else
            Assert (Element (Text, I) = 'L', "Expected 'L' and found '" &
                      Element (Text, I) & "'");
         end if; -- Element (Text, I) = 'R'
         Multiplier := Multiplier / 2;
      end loop; -- I in Positive range 8 .. 10
      This_Seat := Row * Row_Multiplier + Column;
      if This_Seat > Highest_Seat then
         Highest_Seat := This_Seat;
      end if; -- This_Seat > Highest_Seat
      if This_Seat < Lowest_Seat then
         Lowest_Seat := This_Seat;
      end if; -- This_Seat < Lowest_Seat
      Include (Seat_Set, This_Seat);
   end loop; -- not End_Of_File (Input_File)
   Put_Line ("Highest:" & Natural'Image (Highest_Seat));
   for Seat in Seats range Lowest_Seat + 1 .. Highest_Seat - 1 loop
      if not Contains (Seat_Set, Seat) and then
        (Contains (Seat_Set, Seat - 1) and Contains (Seat_Set, Seat + 1)) then
         Put_Line ("Seat:" & Seats'Image (Seat));
      end if; -- not Contains (Seat_Set, Seat) and then ..
   end loop; -- Seat in Seats range Lowest_Seat + 1 .. Highest_Seat - 1
   Close (Input_File);
end December_05;
