with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;

procedure December_10 is

   subtype Jolts is Natural;

   package Jolt_IO is new Ada.Text_IO.Integer_IO (Jolts);

   package Adaptor_Sets is new Ada.Containers.Ordered_Sets (Jolts);
   use Adaptor_Sets;

   Source_Jolts : constant Jolts := 0;
   Tolerance : constant Jolts := 3;

   procedure Search_Options (Available_In : in Adaptor_Sets.Set;
                             Source_Jolts : in Jolts;
                             Target_Jolts : in Jolts;
                             Count : in out Long_Long_Integer) is

      Available : Adaptor_Sets.Set := Copy (Available_In);

   begin -- Search_Options
      if Source_Jolts + Tolerance = Target_Jolts then
         Count := Count + 1;
      else
         for J in Jolts range Source_Jolts + 1 .. Source_Jolts + Tolerance loop
            if Contains (Available, J) then
               Exclude (Available, J);
               Search_Options (Available, J, Target_Jolts, Count);
               Include (Available, J);
            end if; -- Contains (Available, J)
         end loop; -- J in Jolts range Source_Jolts .. Source_Jolts + Tolerance
      end if; -- Source_Jolts + Tolerance = Target_Jolts
   end Search_Options;

   Input_File : File_Type;
   Text : Unbounded_String;
   Adaptor_Set : Adaptor_Sets.Set := To_Set (Source_Jolts);
   Adaptor : Jolts;
   Target_Jolts : Jolts;
   Difference_1, Difference_3 : Natural := 0;
   Count : Long_Long_Integer := 0;

begin -- December_10
   Open (Input_File, In_File, "december_10.txt");
   -- Open (Input_File, In_File, "example_1_10.txt");
   -- Open (Input_File, In_File, "example_2_10.txt");
   while not End_Of_File (Input_File) loop
      Jolt_IO.Get (Input_File, Adaptor);
      Skip_Line (Input_File);
      Include (Adaptor_Set, Adaptor);
   end loop; -- not End_Of_File (Input_File)
   Target_Jolts := Last_Element (Adaptor_Set) + Tolerance;
   Include (Adaptor_Set, Target_Jolts);
   for A in Iterate (Adaptor_Set) loop
      if Next (A) /= No_Element then
         if Element (Next (A)) - Element (A) = 1 then
            Difference_1 := Difference_1 + 1;
         elsif Element (Next (A)) - Element (A) = 3 then
            Difference_3 := Difference_3 + 1;
         end if; -- Element (Next (I)) - Element (I) = 1
      end if; -- Next (A) /= No_Element
   end loop; -- A in Iterate (Adaptor_Set)
   Put_Line ("Difference_1:" & Jolts'Image (Difference_1));
   Put_Line ("Difference_3:" & Jolts'Image (Difference_3));
   Put_Line ("Product:" & Jolts'Image (Difference_1 * Difference_3));
   Search_Options (Adaptor_Set, Source_Jolts, Target_Jolts, Count);
   Put_Line ("Option count:" & Long_Long_Integer'Image (Count));
   Close (Input_File );
end December_10;
