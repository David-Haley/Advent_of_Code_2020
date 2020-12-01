with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_01 is

   Input_File : File_Type;

   package Expenses is new Ada.Containers.Vectors (Positive, Natural);
   use Expenses;
   Expense : Expenses.Vector := Empty_Vector;

begin -- December_01
   Open (Input_File, In_File, "december_01.txt");
   while not End_Of_File (Input_File) loop
      Append (Expense, Natural'Value (Get_Line (Input_File)));
   end loop; -- End_Of_File (Input_File)
   Close (Input_File);
   for I in Positive range 1 .. Last_Index (Expense) - 1 loop
      for J in Positive range I + 1 .. Last_Index (Expense) loop
         if Expense (I) + Expense (J) = 2020 then
            Put_Line ("Part one :" & Natural'Image (Expense (I)) & " *" &
                        Natural'Image (Expense (J)) & " =" &
                        Natural'Image (Expense (I) * Expense (J)));
         else
            for K in Positive range J + 1 .. Last_Index (Expense) loop
               if Expense (I) + Expense (J) + Expense (K) = 2020 then
                  Put_Line ("Part two :" & Natural'Image (Expense (I)) & " *" &
                              Natural'Image (Expense (J)) & " *" &
                              Natural'Image (Expense (K)) & " =" &
                              Natural'Image (Expense (I) *
                                Expense (J) * Expense (K)));
               end if; -- Expense (I) + Expense (J) + Expense (K) = 2020
            end loop; -- K in Positive range I + 2 .. Last_Index (Expense)
         end if; -- Expense (I) + Expense (J) = 2020
      end loop; -- J in Positive range I + 1 .. Last_Index (Expense)
   end loop; -- I in Positive range 1 .. Last_Index (Expense) - 1
end December_01;
