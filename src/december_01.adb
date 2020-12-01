with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_01 is

   Input_File : File_Type;

   -- Assumption there are no 0 expenses
   package Expenses is new Ada.Containers.Vectors (Positive, Positive);
   use Expenses;
   Expense : Expenses.Vector := Empty_Vector;

   Year_2020 : constant Positive := 2020;

begin -- December_01
   Open (Input_File, In_File, "december_01.txt");
   while not End_Of_File (Input_File) loop
      Append (Expense, Positive'Value (Get_Line (Input_File)));
   end loop; -- End_Of_File (Input_File)
   Close (Input_File);
   for I in Positive range 1 .. Last_Index (Expense) loop
      for J in Positive range I + 1 .. Last_Index (Expense) loop
         -- I < J therefore if I = Last_Index the J loop does not execute
         if Expense (I) + Expense (J) = Year_2020 then
            Put_Line ("Part one :" & Positive'Image (Expense (I)) & " *" &
                        Positive'Image (Expense (J)) & " =" &
                        Positive'Image (Expense (I) * Expense (J)));
         else
            -- On the assumption there are no zero expenses once two expenses
            -- add to the required result adding a third can never equal the
            -- required result.
            for K in Positive range J + 1 .. Last_Index (Expense) loop
               -- J > I and K > J implies K > I, that is, I, J and K are unique
               if Expense (I) + Expense (J) + Expense (K) = Year_2020 then
                  Put_Line ("Part two :" & Positive'Image (Expense (I)) & " *" &
                              Positive'Image (Expense (J)) & " *" &
                              Positive'Image (Expense (K)) & " =" &
                              Positive'Image (Expense (I) *
                                Expense (J) * Expense (K)));
               end if; -- Expense (I) + Expense (J) + Expense (K) = Year_2020
            end loop; -- K in Positive range J + 1 .. Last_Index (Expense)
         end if; -- Expense (I) + Expense (J) = Year_2020
      end loop; -- J in Positive range I + 1 .. Last_Index (Expense)
   end loop; -- I in Positive range 1 .. Last_Index (Expense)
end December_01;
