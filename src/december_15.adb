with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Assertions; use Ada.Assertions;

procedure December_15 is

   subtype Numbers is Natural;

   function Solve (Turn_Limit : Positive) return Numbers is

      subtype Spoken_Counts is Natural;
      subtype Turns is Positive range 1 .. Turn_Limit;

      package Number_IO is new Ada.Text_IO.Integer_IO (Numbers);

      type Number_Map_Elements is record
         Turn, Previous_Turn : Turns;
         Spoken_Count : Spoken_Counts := 0;
      end record; -- Number_Map_Elements

      package Number_Maps is new
        Ada.Containers.Ordered_Maps (Numbers, Number_Map_Elements);
      use Number_Maps;

      Input_File : File_Type;
      Ch : Character;
      Number_Map_Element : Number_Map_Elements;
      Number_Map : Number_Maps.Map;
      Last_Spoken : Numbers;
      Turn : Turns := 1;

   begin -- Solve
      Open (Input_File, In_File, "december_15.txt");
      -- Open (Input_File, In_File, "example_15.txt");
      while not End_Of_File (Input_File) loop
         Number_IO.Get (Input_File, Last_Spoken);
         Number_Map_Element.Turn := Turn;
         Include (Number_Map, Last_Spoken, Number_Map_Element);
         Turn := Turn + 1;
         if not End_Of_File (Input_File) then
            Get (Input_File, Ch);
            Assert (Ch = ',', "Expected ',' and found '" & Ch & "'");
         end if; -- not End_Of_File (Input_File)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      loop -- Turn
         if Contains (Number_Map, Last_Spoken) then
            if Number_Map (Last_Spoken).Spoken_Count = 0 then
               Last_Spoken := 0;
            else
               Last_Spoken := Number_Map (Last_Spoken).Turn -
                 Number_Map (Last_Spoken).Previous_Turn;
            end if; -- Number_Map (Last_Spoken).Spoken_Count = 0
         else
            Last_Spoken := 0;
         end if; -- Contains (Number_Map, Last_Spoken)
         if Contains (Number_Map, Last_Spoken) then
            Number_Map (Last_Spoken).Previous_Turn :=
              Number_Map (Last_Spoken).Turn;
            Number_Map (Last_Spoken).Turn := Turn;
            Number_Map (Last_Spoken).Spoken_Count :=
              Number_Map (Last_Spoken).Spoken_Count + 1;
         else
            Number_Map_Element.Turn := Turn;
            Include (Number_Map, Last_Spoken, Number_Map_Element);
         end if; -- Contains (Number_Map, Last_Spoken)
         exit when Turn = Turns'Last;
         Turn := Turn + 1;
      end loop; -- Turn
      return Last_Spoken;
   end Solve;

begin -- December_15
   Put_Line ("Last_Spoken: (Part one):" & Numbers'Image (Solve (2020)));
   Put_Line ("Last_Spoken (Part two):" & Numbers'Image (Solve (30000000)));
end December_15;
