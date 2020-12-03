with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_03 is

   procedure Solve (Input_File : in File_Type;
                    X_Limit, Y_Limit : in Natural) is

      subtype X_Coordinates is Natural range 0 .. X_Limit - 1;
      subtype Y_Coordinates is Natural range 0 .. Y_Limit - 1;
      subtype Slope_Element is Character with
        Static_Predicate => Slope_Element in '.' | '#';

      type Gradient_Element is record
         Right, Down : Positive;
      end record; -- Gradient_Element

      type Slopes is array (X_Coordinates, Y_Coordinates) of Slope_Element;

      function Tree_Count (Gradient : in Gradient_Element;
                           Slope : in Slopes) return Natural is
         X : Natural := 0;
         Y : Y_Coordinates := 0;
         Result : Natural := 0;

      begin -- Tree_Count
      loop -- Y
         if Slope (X mod X_Limit, Y) = '#' then
            Result := Result + 1;
         end if; --  Slope (X mod X_Limit, Y) = '#'
         exit when Y = Y_Coordinates'Last;
         X := X + Gradient.Right;
         Y := Y + Gradient.Down;
         end loop; -- Y
         return Result;
      end Tree_Count;

      Part_One_Gradient : constant Gradient_Element := (Right => 3, Down => 1);

      subtype Gradient_Indices is Positive range 1 .. 5;
      Gradient : constant array (Gradient_Indices) of Gradient_Element :=
        ((Right => 1, Down =>  1),
         (Right => 3, Down => 1),
         (Right => 5, Down => 1),
         (Right => 7, Down => 1),
         (Right => 1, Down => 2));

      Slope : Slopes;
      Tree_Product : Long_Long_Integer := 1;

   begin -- Solve
      for Y in Y_Coordinates loop
         for X in X_Coordinates loop
            Get (Input_File, Slope (X, Y));
         end loop; -- X in X_Coordinates
         Skip_Line (Input_File);
      end loop; -- Y in Y_Coordinates
      Put_Line ("Tree Count:" & Natural'Image (Tree_Count (Part_One_Gradient,
                Slope)));
      Put_Line ("Part Two");
      for G in Gradient_Indices loop
         Tree_Product := Tree_Product *
           Long_Long_Integer (Tree_Count (Gradient (G), Slope));
      end loop; -- G in Gradient_Indices
      Put_Line ("Tree Product:" & Long_Long_Integer'Image (Tree_Product));
   end Solve;

   Input_File : File_Type;
   Text : Unbounded_String;
   X_Limit, Y_Limit : Natural := 0;

begin -- December_03
   Open (Input_File, In_File, "december_03.txt");
   while not End_Of_File (Input_File) loop
      Y_Limit := Y_Limit + 1;
      Get_Line (Input_File, Text);
      if Y_Limit > 1 then
         Assert (X_Limit = Length (Text), "lines not all the same length");
      else
         X_Limit := Length (Text);
      end if; -- Y_Limit > 1
   end loop; -- not End_Of_File (Input_File)
   Reset (Input_File);
   Solve (Input_File, X_Limit, Y_Limit);
   Close (Input_File);
end December_03;
