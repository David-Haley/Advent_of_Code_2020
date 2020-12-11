with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Containers.Ordered_Maps;

procedure December_10 is

   subtype Jolts is Natural;

   package Jolt_IO is new Ada.Text_IO.Integer_IO (Jolts);

   subtype Options is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Adaptor_Maps is new Ada.Containers.Ordered_Maps (Jolts, Options);
   use Adaptor_Maps;

   Source_Jolts : constant Jolts := 0;
   Tolerance : constant Jolts := 3;

   procedure Build_Options (Adaptor_Map : in out Adaptor_Maps.Map) is

      Count : Options;
      Lower, Upper : Jolts;

   begin -- Build_Options
      for A in Iterate (Adaptor_Map) loop
         Count := 0;
         if Key (A) - Tolerance < Source_Jolts then
            Lower := Source_Jolts;
         else
            Lower := Key (A) - Tolerance;
         end if; -- Key (A) - Tolerance < Source_Jolts
         if Key (A) - 1 < Source_Jolts then
            Upper := Source_Jolts;
         else
            Upper := Key (A) - 1;
         end if; -- Adaptor_Map (A) - 1 < Source_Jolts
         for J in Jolts range Lower .. Upper loop
            if Contains (Adaptor_Map, J) then
               Count := Count + Adaptor_Map (J);
            end if; -- Contains (Adaptor_Map, J)
         end loop; -- J in Jolts range Key (A) - Tolerance .. Key (A) - 1
         Adaptor_Map (A) := Count;
      end loop; -- A in Iterate (Adaptor_Map)
   end Build_Options;

   Input_File : File_Type;
   Adaptor_Map : Adaptor_Maps.Map;
   Adaptor, Device_Jolts : Jolts;
   Difference_1, Difference_3 : Natural := 0;

begin -- December_10
   Include (Adaptor_Map, Source_Jolts, 1);
   Open (Input_File, In_File, "december_10.txt");
   while not End_Of_File (Input_File) loop
      Jolt_IO.Get (Input_File, Adaptor);
      Skip_Line (Input_File);
      Include (Adaptor_Map, Adaptor, Options'First);
   end loop; -- not End_Of_File (Input_File)
   Device_Jolts := Last_Key (Adaptor_Map) + Tolerance;
   Include (Adaptor_Map, Device_Jolts, Options'First);
   for A in Iterate (Adaptor_Map) loop
      if Next (A) /= No_Element then
         if Key (Next (A)) - Key (A) = 1 then
            Difference_1 := Difference_1 + 1;
         elsif Key (Next (A)) - Key (A) = 3 then
            Difference_3 := Difference_3 + 1;
         end if; -- Element (Next (I)) - Element (I) = 1
      end if; -- Next (A) /= No_Element
   end loop; -- A in Iterate (Adaptor_Map)
   Put_Line ("Part one:" & Jolts'Image (Difference_1 * Difference_3));
   Build_Options (Adaptor_Map);
   Put_Line ("Part two:" & Options'Image (Element (Adaptor_Map, Device_Jolts)));
   Close (Input_File );
end December_10;
