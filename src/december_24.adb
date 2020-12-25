with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Ordered_Maps;
with NT_Console;

procedure December_24 is

   type Directions is (e, se, sw, w, nw, ne);
   subtype Direction_Characters is Character with
     Static_Predicate => Direction_Characters in 'e' | 'n' | 's' | 'w';

   subtype Colour is Boolean;
   Black : constant Colour := False;
   White : constant Colour := True;

   type Coordinates is record
      X, Y : Integer;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is

   begin -- "<"
      return Left.X < Right.X or else
        (Left.X = Right.X and then Left.Y < Right.Y);
   end "<";

   package Hex_Planes is new Ada.Containers.Ordered_Maps (Coordinates, Colour);
   use Hex_Planes;

   function Step (Old : in Coordinates;
                  Direction : in Directions) return Coordinates is

      Next : Coordinates := Old;

   begin -- Step
      Assert (Old.X mod 2 = Old.Y mod 2, "Old non hex coordinate");
      case Direction is
         when e =>
            Next.X := Old.X + 2;
         when w =>
            Next.X := Old.X - 2;
         when se =>
            Next.X := Old.X + 1;
            Next.Y := Old.Y - 1;
         when sw =>
            Next.X := Old.X - 1;
            Next.Y := Old.Y - 1;
         when ne =>
            Next.X := Old.X + 1;
            Next.Y := Old.Y + 1;
         when nw =>
            Next.X := Old.X - 1;
            Next.Y := Old.Y + 1;
      end case; -- Direction
      Assert (Next.X mod 2 = Next.Y mod 2, "Next non hex coordinate");
      return Next;
   end Step;

   procedure Read_Floor (Input_File : in File_Type;
                         Floor : out Hex_Planes.Map) is

      Ch : Direction_Characters;
      Direction : Directions;
      Current : Coordinates;

   begin -- Read_Floor
      while not End_Of_File (Input_File) loop
         Current := (0, 0);
         while not End_Of_Line (Input_File) loop
            Get (Input_File, Ch);
            case Ch is
            when 'e' =>
               Direction := e;
            when 'n' =>
               Get (Input_File, Ch);
               Assert (Ch = 'e' or Ch = 'w',
                       "After 'n' Eepected 'e' or 'w' and found '" & Ch &
                         "' at column" & Col (Input_File)'Img &
                         " line" & line (Input_File)'Img);
               if Ch = 'e' then
                  Direction := ne;
               else
                  Direction := nw;
               end if; -- Ch = 'e'
            when 's' =>
               Get (Input_File, Ch);
               Assert (Ch = 'e' or Ch = 'w',
                       "After 's' Eepected 'e' or 'w' and found '" & Ch &
                         "' at column" & Col (Input_File)'Img &
                         " line" & line (Input_File)'Img);
               if Ch = 'e' then
                  Direction := se;
               else
                  Direction := sw;
               end if; -- Ch = 'e'
            when 'w' =>
               Direction := w;
            end case; -- Ch
            Current := Step (Current, Direction);
         end loop; --while not End_Of_Line (Input_File)
         if Contains (Floor, Current) then
            Floor (Current) := not Floor (Current);
         else
            Include (Floor, Current, Black);
         end if; -- Conntains (Floor, Current)
         Skip_Line (Input_File);
      end loop; -- not End_Of_File (Input_File)
   end Read_Floor;

   function Animate_Floor (Floor : in Hex_Planes.Map) return Hex_Planes.Map is

      function Black_Count (Floor : in Hex_Planes.Map;
                            Test_Coordinate : in Coordinates) return Natural is

         Count : Natural := 0;
         Count_Coordinate : Coordinates;

      begin -- Black_Count
         for Direction in Directions loop
            Count_Coordinate := Step (Test_Coordinate, Direction);
            -- Tiles not in map are by definition white
            if Contains (Floor, Count_Coordinate) and then
              Floor (Count_Coordinate) = Black then
               Count := Count + 1;
            end if; -- Cointains (Floor, Count_Coordinate) and then ...
         end loop; -- Direction in Directions
         return Count;
      end Black_Count;

      Next_Floor : Hex_Planes.Map := Copy (Floor);
      Neighbour : Coordinates;

   begin -- Animate_Floor
      for I in Iterate (Floor) loop
         -- All known tiles were black sometime
         if Floor (I) = Black then
            if Black_Count (Floor, Key (I)) = 0 or
              Black_Count (Floor, Key (I)) > 2 then
               Next_Floor (Key (I)) := White;
            end if; -- Black_Count (Floor, Key (I)) = 0 or ...
         else
            if Black_Count (Floor, Key (I)) = 2 then
               Next_Floor (Key (I)) := Black;
            end if; -- Floor (I) = Black
         end if; -- Floor (I) = Black
         for D in Directions loop
            -- examine tiles adjacent to known tiles
            Neighbour := Step (Key (I), D);
            -- Neighbour may not currently be in map
            if Contains (Floor, Neighbour) then
               if Floor (Neighbour) = Black then
                  if Black_Count (Floor, Neighbour) = 0 or
                    Black_Count (Floor, Neighbour) > 2 then
                     Next_Floor (Neighbour) := White;
                  end if; -- Black_Count (Floor, Key (I)) = 0 or ...
               else
                  if Black_Count (Floor, Neighbour) = 2 then
                     Next_Floor (Neighbour) := Black;
                  end if; -- Floor (I) = Black
               end if; -- Floor (I) = Black
            else
               -- not in map therefore must be white.
               if Black_Count (Floor, Neighbour) = 2 then
                  Include (Next_Floor, Neighbour, Black);
               end if; -- Black_Count (Floor, Neighbour) = 2
            end if; -- Contains (Floor, Neighbour)
         end loop; -- D in Directions
      end loop; -- I in Iterate (Floor)
      return Next_Floor;
   end Animate_Floor;

   function Black_Count (Floor : in Hex_Planes.Map) return Natural is

      Count : Natural := 0;

   begin -- Black_Count
      for I in Iterate (Floor) loop
         if Floor (I) = Black then
            Count := Count + 1;
         end if; -- Floor (I) = Black
      end loop; -- I in Iterate (Floor)
      return Count;
   end Black_Count;

   procedure Display (Input_File : in File_Type;
                      X_Min, Y_Min, X_Max, Y_Max : in Integer) is

      package Screen is new NT_Console (X_Max - X_Min + 1, Y_Max - Y_Min + 1);
      use Screen;

      procedure Put (Floor : in Hex_Planes.Map) is

      begin -- Put
         for I in Iterate (Floor) loop
            Goto_XY (Key (I).X - X_Min, Key (I).Y - Y_Min);
            if Floor (I) = Black then
               Set_Foreground (Screen.Black);
            else
               Set_Foreground (Screen.White);
            end if; -- Next_Floor (I) = Black
            Put ('*');
         end loop; -- I in Iterate (Next_Floor)
         Set_Foreground (Gray);
      end Put;

      Time_Increment : constant Duration := 1.0;
      Next : Time;
      Floor, Next_Floor : Hex_Planes.Map := Hex_Planes.Empty_Map;

   begin -- Display
      Read_Floor (Input_File, Floor);
      Clear_Screen;
      Next := Clock + Time_Increment;
      for Day in Positive range 1 .. 100 loop
         Next_Floor := Animate_Floor (Floor);
         Floor := Copy (Next_Floor);
         Put (Floor);
         delay until Next;
         Next := Next + Time_Increment;
      end loop; -- Day in Positive range 1 .. 100
      Goto_XY (0, Y_Pos'Last);
   end Display;

   Input_File : File_Type;
   Floor, Next_Floor : Hex_Planes.Map := Hex_Planes.Empty_Map;
   X_Min, Y_Min : Integer := Integer'Last;
   X_Max, Y_Max : Integer := Integer'First;
   Ch : Character;

begin -- December_24
   Open (Input_File, In_File, "december_24.txt");
   Read_Floor (Input_File, Floor);
   Put_Line ("Black Tiles (Part one):" & Black_Count (Floor)'Img);
   for Day in Positive range 1 .. 100 loop
      Next_Floor := Animate_Floor (Floor);
      Floor := Copy (Next_Floor);
   end loop; -- Day in Positive range 1 .. 100
   Put_Line ("Black Tiles (Part two):" & Black_Count (Floor)'Img);
   for I in Iterate (Floor) loop
      if Key (I).X < X_Min then
         X_Min := Key (I).X;
      end if; -- Key (I).X < X_Min
      if Key (I).X > X_Max then
         X_Max := Key (I).X;
      end if; -- Key (I).X > X_Max
      if Key (I).Y < Y_Min then
         Y_Min := Key (I).Y;
      end if; -- Key (I).Y < Y_Min
      if Key (I).Y > Y_Max then
         Y_Max := Key (I).Y;
      end if; -- Key (I).Y > Y_Max
   end loop; -- I in Iterate (Floor)
   Put_Line ("Terminal size is (" & Positive'Image(X_Max - X_Min + 1) & ',' &
               Positive'Image (Y_Max - Y_Min + 1) & ')');
   Put_Line ("To run amimated display press 'd' and enter: ");
   Get (Ch);
   if Ch = 'd' or Ch = 'D' then
      Reset (Input_File);
      Display (Input_File, X_Min, Y_Min, X_Max, Y_Max);
   end if; -- Ch = 'd' or Ch = 'D'
   Close (Input_File);
end December_24;
