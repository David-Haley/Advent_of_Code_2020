with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Assertions; use Ada.Assertions;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;
with NT_Console;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_20 is

   subtype Pixel_Coordinates is Natural range 0 .. 9; -- from example and input

   type Images is array (Pixel_Coordinates, Pixel_Coordinates) of Boolean;

   type Edge_Indices is mod 4;
   type Single_Edges is array (Pixel_Coordinates) of Boolean;
   type Edge_Arrays is array (Edge_Indices) of Single_Edges;

   Adjacent : constant Edge_Indices := 2;
   -- adding this to an edge index convertes it to the edge index an adjoining
   -- tile.

   subtype Orientations is Natural range 0 .. 7;

   subtype Tile_Coordinates is Natural;
   subtype Tile_Indices is Positive;

   type Match_Elements is record
      Tile_Index : Tile_Indices; -- Other Tile match
      Orientation : Orientations; -- Other Tile's Orientation matches
      Matched_Edge : Edge_Indices; -- This tile's edge that has been matched
   end record; -- Match_Elements

   package Match_Lists is new Ada.Containers.Vectors (Positive, Match_Elements);
   use match_Lists;

   type Trial_Element is record
      Image : Images;
      Edge_Array : Edge_Arrays;
      Match_List : Match_Lists.Vector := Match_Lists.Empty_Vector;
   end record; -- Trial_Element

   type Trial_Arrays is array (Orientations) of Trial_Element;
   subtype Tile_IDs is Unsigned_64;

   type Tiles is record
      Tile_ID : Tile_IDs;
      Trial_Array : Trial_Arrays;
   end record; -- Tiles

   package Tile_Stores is new Ada.Containers.Vectors (Tile_Indices, Tiles);
   use Tile_Stores;

   package Screen is new NT_Console (120, 120);
   use Screen;

   procedure Read_Tiles (Tile_Store : out Tile_Stores.Vector) is

      Input_File :  File_Type;
      Text : Unbounded_String;
      First : Positive;
      Last : Natural;
      Ch : Character;
      Tile : Tiles;

   begin -- Read_Tiles
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_20.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Find_Token (Text, Decimal_Digit_Set, Inside, First, Last);
         if Last > 0 then
            Tile.Tile_ID := Tile_IDs'Value (Slice (Text, First, Last));
            for Y in Pixel_Coordinates loop
               for X in Pixel_Coordinates loop
                  Get (Input_File, Ch);
                  Tile.Trial_Array (Orientations'First).Image (X, Y) :=
                    Ch = '#';
               end loop; -- X in Pixel_Coordinates
               Skip_Line (Input_File);
            end loop; -- Y in Pixel_Coordinates
            Append (Tile_Store, Tile);
         end if; -- Last > 0;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Tiles;

   function Transform (Image : in Images;
                       Orientation : Orientations) return Images is

      type Axies is (X, Y);

      subtype Rotations is Natural range 0 .. 3;

      subtype Flips is Boolean;

      subtype Transform_Elements is Integer range -1 .. 1;

      type Transform_Matrix is Array (Axies, Axies) of Transform_Elements;

      function Basic_Transform (Image : in Images;
                                Matrix : in Transform_Matrix) return Images is

         type Coordinates is array (Axies) of Pixel_Coordinates;

         C_In, C_Out : Coordinates;

         Result : Images;

      begin -- Basic_Transform
         for Xi in Pixel_Coordinates loop
            for Yi in Pixel_Coordinates loop
               C_In := (Xi, Yi);
               for Axis_In in Axies loop
                  for Axis_Out in Axies loop
                     if Matrix (Axis_In, Axis_Out) = 1 then
                        C_Out (Axis_Out) := C_In (Axis_In);
                     elsif Matrix (Axis_In, Axis_Out) = -1 then
                        C_Out (Axis_Out) :=
                          Pixel_Coordinates'Last - C_In (Axis_In);
                     end if; -- Matrix (Axis_In, Axis_Out) = 1
                  end loop; --  Axis_Out in Axies
               end loop; -- Axis_In in Axies
               Result (C_Out (X), C_Out (Y)) := Image (C_In (X), C_In (Y));
            end loop; -- Yi in Pixel_Coordinates
         end loop; -- Xi in Pixel_Coordinates
         return Result;
      end Basic_Transform;

      Rotation_Table : constant array (Rotations) of Transform_Matrix :=
        (
         00 => (( 1,  0),
                ( 0,  1)), -- no rotation
         01 => (( 0, -1),
                ( 1,  0)),
         02 => ((-1,  0),
                ( 0, -1)),
         03 => (( 0,  1),
                (-1,  0)));

      X_Flip : constant Transform_Matrix :=
        (( 1,  0),
         ( 0, -1));

      Result : Images;

   begin -- Transform
      Result := Basic_Transform (Image, Rotation_Table (Orientation mod
                                   (Rotations'Last + 1)));
      if Orientation / (Rotations'Last + 1) > 0 then
            Result := Basic_Transform (Result, X_Flip);
      end if; -- Orientation / (Rotations'Last + 1) > 0
      return Result;
   end Transform;

   procedure Build_Trials (Tile_Store : in out Tile_Stores.Vector) is

   begin -- Build_Trials
      for T in Iterate (Tile_Store) loop
         -- Create rotated and flipped images
         for O in Orientations range Orientations'First + 1 ..
           Orientations'Last loop
            Tile_Store (T).Trial_Array (O).Image :=
              Transform (Tile_Store (T).Trial_Array (Orientations'First).Image,
                         O);
         end loop; -- O in Orientations range Orientations'First + 1 ...
         -- Create Edge_Array
         for O in Orientations loop
            for P in Pixel_Coordinates loop
               Tile_Store (T).Trial_Array (O).Edge_Array (0) (P) :=
                 Tile_Store (T).Trial_Array (O).
                 Image (P, Pixel_Coordinates'First);
               Tile_Store (T).Trial_Array (O).Edge_Array (1) (P) :=
                 Tile_Store (T).Trial_Array (O).
                 Image (Pixel_Coordinates'Last, P);
               Tile_Store (T).Trial_Array (O).Edge_Array (2) (P) :=
                 Tile_Store (T).Trial_Array (O).
                 Image (P, Pixel_Coordinates'Last);
               Tile_Store (T).Trial_Array (O).Edge_Array (3) (P) :=
                 Tile_Store (T).Trial_Array (O).
                 Image (Pixel_Coordinates'First, P);
            end loop; -- P in Pixel_Coordinates
         end loop; -- O in Orientations
      end loop; -- T in Iterate (Tile_Store)
   end Build_Trials;

   procedure Build_Matches (Tile_Store : in out Tile_Stores.Vector) is

      Match_Element : Match_Elements;

   begin -- Build_Matches
      for T1 in Tile_Indices range Tile_Indices'First ..
        Last_Index (Tile_Store) - 1 loop
         for T2 in Tile_Indices range T1 + 1 .. Last_Index (Tile_Store) loop
            for O1 in Orientations loop
               for O2 in Orientations loop
                  for E in Edge_Indices loop
                     if Tile_Store (T1).Trial_Array (O1).Edge_Array (E) =
                       Tile_Store (T2).Trial_Array (O2).Edge_Array
                       (E + Adjacent) then
                        Match_Element := (Tile_Index => T2,
                                          Orientation => O2,
                                          Matched_Edge => E);
                        Append (Tile_Store (T1).Trial_Array (O1).Match_list,
                                Match_Element);
                        Match_Element := (Tile_Index => T1,
                                          Orientation => O1,
                                          Matched_Edge => E + Adjacent);
                        Append (Tile_Store (T2).Trial_Array (O1).Match_list,
                                Match_Element);
                     end if; -- Tile_Store (T1).Trial_Array (O1).Edge_Array ...
                  end loop; -- E in Edge_Indices
               end loop; -- O2 in Orientations
            end loop; -- O1 in Orientations
         end loop; --  T2 in Tile_Indices range T1 + 1 ...
      end loop; -- T1 in Tile_Indices range Tile_Indices'First ...
   end Build_Matches;

   procedure Put (Image : in Images;
                  X_Offset : in X_Pos;
                  Y_Offset : in Y_Pos) is

   begin -- Put
      for Y in Pixel_Coordinates loop
         Goto_XY (X_Offset, Y_Offset + Y);
         for X in Pixel_Coordinates loop
            if Image (X, Y) then
               Put ('#');
            else
               Put ('.');
            end if; -- Image (X, Y)
         end loop; -- X in Pixel_Coordinates
      end loop; -- Y in Pixel_Coordinates
   end Put;

   Tile_Store : Tile_Stores.Vector;
   Product : Tile_IDs := 1;

begin -- December_20
   Read_Tiles (Tile_Store);
   Build_Trials (Tile_Store);
   Build_Matches (Tile_Store);
   for T in Iterate (Tile_Store) loop
      if Length (Tile_Store (T).Trial_Array (0).Match_list) = 2 then
         Product := Product * Tile_Store (T).Tile_ID;
      end if; -- Length (Tile_Store (T).Trial_Array (0).Match_list) = 2
   end loop; -- T in Iterate (Tile_Store)
   Put_Line ("Part One:" & Product'Img);
   Put_CPU_Time;
end December_20;
