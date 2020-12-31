with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure December_21 is

   subtype Allergens is Unbounded_String;
   subtype Ingredients is Unbounded_String;

   package Ingredient_Sets is new Ada.Containers.Ordered_Sets (Ingredients);
   use Ingredient_Sets;

   package Ingredient_Lists is new
     Ada.Containers.Vectors (Positive, Ingredient_Sets.Set);
   use Ingredient_Lists;

   type Allegen_Elements is record
      Ingredient_List : Ingredient_Lists.Vector :=
        Ingredient_Lists.Empty_Vector;
      Possible_Allergen_Set : Ingredient_Sets.Set := Ingredient_Sets.Empty_Set;
      Actual : Ingredient_Sets.Set := Ingredient_Sets.Empty_Set;
   end record; -- Allegen_Elements

   package Allergen_Lists is new
     Ada.Containers.Ordered_Maps (Allergens, Allegen_Elements);
   use Allergen_Lists;

   procedure Read_List (Input_File : in File_Type;
                        Allergen_List : out Allergen_Lists.Map;
                        Ingredient_List : out Ingredient_Lists.Vector) is

      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Ingredient_Set : Ingredient_Sets.Set;
      Allegen_Element : Allegen_Elements;

   begin -- Read_List
      Allergen_List := Allergen_Lists.Empty_Map;
      Ingredient_List := Ingredient_Lists.Empty_Vector;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Clear (Ingredient_Set);
         loop -- Ingredients
            Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
            Start_At := Last + 1;
            exit when Slice (Text, First, Last) = "contains";
            Include (Ingredient_Set, Unbounded_Slice (Text, First, Last));
         end loop; -- Ingredients
         Append (Ingredient_List, Ingredient_Set);
         while Element (Text, Start_At) /= ')' loop
            Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
            Start_At := Last + 1;
            if not Contains (Allergen_List,
                             Unbounded_Slice (Text, First, Last)) then
               Include (Allergen_List, Unbounded_Slice (Text, First, Last),
                        Allegen_Element);
            end if; -- Contains (Allergen_List, ...
            Append (Allergen_List (Unbounded_Slice (Text, First, Last)).
                      Ingredient_List, Ingredient_Set);
         end loop; -- Element (Text, Start_At) /= ')'
      end loop; -- not End_Of_File (Input_File)
   end Read_List;

   procedure Build_Possible (Allergen_List : in out Allergen_Lists.Map;
                            All_Ingredients : out Ingredient_Sets.Set) is

   begin -- Build_Possible
      All_Ingredients := Ingredient_Sets.Empty_Set;
      for A in Iterate (Allergen_List) loop
         Allergen_List (A).Possible_Allergen_Set :=
           First_Element (Allergen_List (A).Ingredient_List);
         for L in Iterate (Allergen_List (A).Ingredient_List) loop
            Allergen_List (A).Possible_Allergen_Set :=
              Intersection (Allergen_List (A).Possible_Allergen_Set,
                            Allergen_List (A).Ingredient_List (L));
            All_Ingredients := Union (All_Ingredients,
                                      Allergen_List (A).Ingredient_List (L));
         end loop; -- L in Iterate (Allergen_List (A).Ingredient_List)
      end loop; -- A in Iterate (Allergen_List)
   end Build_Possible;

   procedure Build_Actual (Allergen_List : in out Allergen_Lists.Map) is

      function All_One (Allergen_List : in Allergen_Lists.Map)
                        return Boolean is

         Result : Boolean := True;

      begin -- All_One
         for A in Iterate (Allergen_List) loop
            Result := Result and
              Length (Allergen_List (A).Actual) = 1;
         end loop; -- A in Iterate (Allergen_List)
         return Result;
      end All_One;

   begin -- Build_Actual
      for A in Iterate (Allergen_List) loop
         Allergen_List (A).Actual :=
           Copy (Allergen_List (A).Possible_Allergen_Set);
      end loop; -- A in Iterate (Allergen_List)
      loop -- All One
         for A in Iterate (Allergen_List) loop
            for B in Iterate (Allergen_List) loop
               if Key (A) /= Key (B) and
                 Length (Allergen_List (A).Actual) = 1 then
                  Difference (Allergen_List (B).Actual,
                              Allergen_List (A).Actual);
               end if; -- Key (A) /= Key (B)
            end loop; -- B in Iterate (Allergen_List)
         end loop; -- A in Iterate (Allergen_List)
         exit when All_One (Allergen_List);
      end loop; -- All One
   end Build_Actual;

   Input_File : File_Type;
   Allergen_List : Allergen_Lists.Map;
   Ingredient_List : Ingredient_Lists.Vector;
   All_Ingredients, Safe_Ingredients : Ingredient_Sets.Set;
   Count : Count_Type := 0;

begin -- December_21
   Open (Input_File, In_File, "december_21.txt");
   Read_List (Input_File, Allergen_List, Ingredient_List);
   Close (Input_File);
   Build_Possible (Allergen_List, All_Ingredients);
   Safe_Ingredients := Copy (All_Ingredients);
   for A in Iterate (Allergen_List) loop
      Difference (Safe_Ingredients, Allergen_List (A).Possible_Allergen_Set);
   end loop; -- A in Iterate (Allergen_List)
   for I in Iterate (Ingredient_List) loop
      Count := Count +
        Length (Intersection (Safe_Ingredients, Ingredient_List (I)));
   end loop; -- I in Iterate (Ingredient_List)
   Put_Line ("Safe ingredients (Part one):" & Count'Img);
   Build_Actual (Allergen_List);
   Put ("canonical dangerous ingredient list (Part two): ");
   for A in Iterate (Allergen_List) loop
      Put (First_Element (Allergen_List (A).Actual)); -- note only one element
      if A /= Last (Allergen_List) then
         Put (',');
      else
         New_Line;
      end if; -- A /= Last (Allergen_List)
   end loop; -- A in Iterate (Allergen_List)
end December_21;
