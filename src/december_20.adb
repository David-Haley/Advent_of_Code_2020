with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Assertions; use Ada.Assertions;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Interfaces; use Interfaces;
with NT_Console;
with DJH.Execution_Time; use DJH.Execution_Time;
with Transformations_2D;

procedure December_20 is

   subtype Pixel_Coordinates is Natural range 0 .. 9; -- from example and input

   package Tile_Images is new Transformations_2D (Pixel_Coordinates);
   use Tile_Images;

   type Edge_Indices is mod 4;
   type Single_Edges is array (Pixel_Coordinates) of Boolean;
   type Edge_Arrays is array (Edge_Indices) of Single_Edges;

   Adjacent : constant Edge_Indices := 2;
   -- adding this to an edge index convertes it to the edge index an adjoining
   -- tile.

   subtype Tile_Indices is Positive;

   type Match_Elements is record
      Tile_Index : Tile_Indices; -- Other Tile match
      Orientation : Orientations; -- Other Tile's Orientation matches
      Matched_Edge : Edge_Indices; -- This tile's edge that has been matched
   end record; -- Match_Elements

   package Match_Lists is new Ada.Containers.Vectors
     (Positive, Match_Elements);
   use match_Lists;

   type Trial_Element is record
      Image : Tile_Images.Images;
      Edge_Array : Edge_Arrays;
      Match_List : Match_Lists.Vector := Match_Lists.Empty_Vector;
   end record; -- Trial_Element

   type Trial_Arrays is array (Orientations) of Trial_Element;
   subtype Tile_IDs is Unsigned_64;

   type Tiles is record
      Tile_ID : Tile_IDs;
      Trial_Array : Trial_Arrays;
      Placed : Boolean := False;
   end record; -- Tiles

   package Tile_Stores is new Ada.Containers.Vectors (Tile_Indices, Tiles);
   use Tile_Stores;

   procedure Read_Tiles (Tile_Store : out Tile_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      First : Positive;
      Last : Natural;
      Ch : Character;
      Tile : Tiles;

   begin -- Read_Tiles
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_20.txt");
      else
         Open (Input_File, In_File, Argument (1));
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
               if not End_Of_File (Input_File) then
                  Skip_Line (Input_File);
               end if; -- not End_Of_File (Input_File)
            end loop; -- Y in Pixel_Coordinates
            Append (Tile_Store, Tile);
         end if; -- Last > 0;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Tiles;

   procedure Build_Trials (Tile_Store : in out Tile_Stores.Vector) is

   begin -- Build_Trials
      for T in Iterate (Tile_Store) loop
         -- Create rotated and flipped images
         for O in Orientations range Orientations'First + 1 ..
             Orientations'Last
         loop
            Tile_Store (T).Trial_Array (O).Image :=
              Transform
                (Tile_Store (T).Trial_Array (Orientations'First).Image, O);
         end loop; -- O in Orientations range Orientations'First + 1 ...
         -- Create Edge_Array
         for O in Orientations loop
            for P in Pixel_Coordinates loop
               Tile_Store (T).Trial_Array (O).Edge_Array (0) (P) :=
                 Tile_Store (T).Trial_Array (O).Image
                   (P, Pixel_Coordinates'First);
               Tile_Store (T).Trial_Array (O).Edge_Array (1) (P) :=
                 Tile_Store (T).Trial_Array (O).Image
                   (Pixel_Coordinates'Last, P);
               Tile_Store (T).Trial_Array (O).Edge_Array (2) (P) :=
                 Tile_Store (T).Trial_Array (O).Image
                   (P, Pixel_Coordinates'Last);
               Tile_Store (T).Trial_Array (O).Edge_Array (3) (P) :=
                 Tile_Store (T).Trial_Array (O).Image
                   (Pixel_Coordinates'First, P);
            end loop; -- P in Pixel_Coordinates
         end loop; -- O in Orientations
      end loop; -- T in Iterate (Tile_Store)
   end Build_Trials;

   procedure Build_Matches (Tile_Store : in out Tile_Stores.Vector) is

      Match_Element : Match_Elements;

   begin -- Build_Matches
      for T1 in Tile_Indices range Tile_Indices'First ..
          Last_Index (Tile_Store) - 1
      loop
         for T2 in Tile_Indices range T1 + 1 .. Last_Index (Tile_Store) loop
            for O1 in Orientations loop
               for O2 in Orientations loop
                  for E in Edge_Indices loop
                     if Tile_Store (T1).Trial_Array (O1).Edge_Array (E) =
                       Tile_Store (T2).Trial_Array (O2).Edge_Array
                         (E + Adjacent)
                     then
                        Match_Element :=
                          (Tile_Index => T2, Orientation => O2,
                           Matched_Edge => E);
                        Append
                          (Tile_Store (T1).Trial_Array (O1).Match_list,
                           Match_Element);
                        Match_Element :=
                          (Tile_Index => T1, Orientation => O1,
                           Matched_Edge => E + Adjacent);
                        Append
                          (Tile_Store (T2).Trial_Array (O2).Match_list,
                           Match_Element);
                     end if; -- Tile_Store (T1).Trial_Array (O1).Edge_Array ...
                  end loop; -- E in Edge_Indices
               end loop; -- O2 in Orientations
            end loop; -- O1 in Orientations
         end loop; --  T2 in Tile_Indices range T1 + 1 ...
      end loop; -- T1 in Tile_Indices range Tile_Indices'First ...
   end Build_Matches;

   function Part_Two
     (Tile_Store : in out Tile_Stores.Vector; Side_Length : in Positive)
      return Natural
   is

      subtype Tile_Coordinates is Natural range 0 .. Side_Length - 1;

      subtype Image_Coordinates is
        Natural range 0 ..
            (Pixel_Coordinates'Last - 1) * (Tile_Coordinates'Last + 1) - 1;
      Tile_Offset : constant Image_Coordinates := Pixel_Coordinates'Last - 1;
      -- Edges are not included in assembled image.

      package Pictures is new Transformations_2D (Image_Coordinates);
      use Pictures;

      package Screen is new NT_Console
        (Image_Coordinates'Last, Image_Coordinates'Last);
      use Screen;

      type Monster_Pixels is record
         X, Y : Image_Coordinates;
      end record; -- Monster_Pixels

      package Monster_Stores is new Ada.Containers.Vectors
        (Positive, Monster_Pixels);
      use Monster_Stores;

      type Monsters is record
         Monster_Store : Monster_Stores.Vector;
         X_Limit, Y_Limit : Image_Coordinates;
         Pixel_Count : Natural;
      end record;

      function Assemble
        (Tile_Store : in out Tile_Stores.Vector; Side_Length : in Positive)
         return Pictures.Images
      is

         Right : constant Edge_Indices := 1;
         Bottom : constant Edge_Indices := 2;

         type Assembly_Element is record
            Tile_Index : Tile_Indices;
            Orientation : Pictures.Orientations;
            X, Y : Tile_Coordinates := Tile_Coordinates'First;
         end record; -- Assembly_Element

         function Top_Left_Corner
           (Tile_Store : in Tile_Stores.Vector) return Assembly_Element
         is

            Bottom_Found, Right_Found : Boolean;
            Result : Assembly_Element;

         begin -- Top_Left_Corner
            for T in Iterate (Tile_Store) loop
               for O in Tile_Images.Orientations loop
                  if Length (Tile_Store (T).Trial_Array (O).Match_list) = 2
                  then
                     Bottom_Found := False;
                     Right_Found := False;
                     for M in Iterate
                       (Tile_Store (T).Trial_Array (O).Match_list)
                     loop
                        if Tile_Store (T).Trial_Array (O).Match_list (M)
                            .Matched_Edge =
                          Bottom
                        then
                           Bottom_Found := True;
                        elsif Tile_Store (T).Trial_Array (O).Match_list (M)
                            .Matched_Edge =
                          Right
                        then
                           Right_Found := True;
                        end if; -- Tile_Store (T).Trial_Array (O).Match_list ...
                        if Bottom_Found and Right_Found then
                           Result.Tile_Index := To_Index (T);
                           Result.Orientation := O;
                        end if; -- Bottom_Found and Left_Found
                     end loop; -- M in Iterate (Tile_Store (T).Trial_Array ...
                  end if; -- Length (Tile_Store (T).Trial_Array (O) ...
                  exit when Bottom_Found and Right_Found;
               end loop; -- O in Tile_Images.Orientation
               exit when Bottom_Found and Right_Found;
            end loop; -- T in Iterate (Tile_Store)
            return Result;
         end Top_Left_Corner;

         procedure Transfer
           (Tile_Store : in Tile_Stores.Vector; Place : in Assembly_Element;
            Picture : in out Pictures.Images)
         is

            -- transfers tile image without edges to picture

            X_Offset : Image_Coordinates := Tile_Offset * Place.X;
            Y_Offset : Image_Coordinates := Tile_Offset * Place.Y;

         begin -- Transfer
            for X in Pixel_Coordinates range Pixel_Coordinates'First ..
                Pixel_Coordinates'Last - 2
            loop
               for Y in Pixel_Coordinates range Pixel_Coordinates'First ..
                   Pixel_Coordinates'Last - 2
               loop
                  Picture (X + X_Offset, Y + Y_Offset) :=
                    Tile_Store (Place.Tile_Index).Trial_Array
                      (Place.Orientation)
                      .Image
                      (X + 1, Y + 1);
               end loop; -- Y in Pixel_Coordinates range Pixel_Coordinates'First
            end loop; -- X in Pixel_Coordinates range Pixel_Coordinates'First
         end Transfer;

         package QI is new Ada.Containers.Synchronized_Queue_Interfaces
           (Assembly_Element);

         package Assembly_Qs is new Ada.Containers
           .Unbounded_Synchronized_Queues
           (QI);

         Picture : Pictures.Images := (others => (others => False));
         This_Tile, Next_Tile : Assembly_Element;
         Assembly_Q : Assembly_Qs.Queue;

      begin -- Assemble
         This_Tile := Top_Left_Corner (Tile_Store);
         Assembly_Q.Enqueue (This_Tile);
         while Assembly_Q.Current_Use > 0 loop
            Assembly_Q.Dequeue (This_Tile);
            if not Tile_Store (This_Tile.Tile_Index).Placed then
               Transfer (Tile_Store, This_Tile, Picture);
               Tile_Store (This_Tile.Tile_Index).Placed := True;
               for M in Iterate
                 (Tile_Store (This_Tile.Tile_Index).Trial_Array
                    (This_Tile.Orientation)
                    .Match_List)
               loop
                  if Tile_Store (This_Tile.Tile_Index).Trial_Array
                      (This_Tile.Orientation)
                      .Match_list
                      (M)
                      .Matched_Edge =
                    Bottom
                  then
                     Next_Tile.Tile_Index :=
                       Tile_Store (This_Tile.Tile_Index).Trial_Array
                         (This_Tile.Orientation)
                         .Match_list
                         (M)
                         .Tile_Index;
                     Next_Tile.Orientation :=
                       Tile_Store (This_Tile.Tile_Index).Trial_Array
                         (This_Tile.Orientation)
                         .Match_list
                         (M)
                         .Orientation;
                     Next_Tile.X := This_Tile.X;
                     Next_Tile.Y := This_Tile.Y + 1;
                     Assembly_Q.Enqueue (Next_Tile);
                  elsif Tile_Store (This_Tile.Tile_Index).Trial_Array
                      (This_Tile.Orientation)
                      .Match_list
                      (M)
                      .Matched_Edge =
                    Right
                  then
                     Next_Tile.Tile_Index :=
                       Tile_Store (This_Tile.Tile_Index).Trial_Array
                         (This_Tile.Orientation)
                         .Match_list
                         (M)
                         .Tile_Index;
                     Next_Tile.Orientation :=
                       Tile_Store (This_Tile.Tile_Index).Trial_Array
                         (This_Tile.Orientation)
                         .Match_list
                         (M)
                         .Orientation;
                     Next_Tile.X := This_Tile.X + 1;
                     Next_Tile.Y := This_Tile.Y;
                     Assembly_Q.Enqueue (Next_Tile);
                  end if; -- Tile_Store (This_Tile.Tile_Index). ...
               end loop; --  Iterate (Tile_Store(This_Tile.Tile_Index). ...
            end if; -- not Tile_Store (This_Tile.Tile_Index).Placed
         end loop; -- Assembly_Q.Current_Use > 0
         return Picture;
      end Assemble;

      function Build_Monster return Monsters is

         subtype Mx is Image_Coordinates range Image_Coordinates'First .. 19;
         subtype My is Image_Coordinates range Image_Coordinates'First .. 2;

         Image : constant array (My) of String (1 .. Mx'Last + 1) :=
           (("                  # "),
            ("#    ##    ##    ###"),
            (" #  #  #  #  #  #   "));

         Monster_Pixel : Monster_Pixels;
         Monster : Monsters :=
           (Monster_Store => Monster_Stores.Empty_Vector, X_Limit => Mx'Last,
            Y_Limit => My'Last, Pixel_Count => 0);

      begin -- Build_Monster
         for Y in My loop
            for X in Mx loop
               if Image (Y) (X + 1) = '#' then
                  Monster_Pixel := (X, Y);
                  Append (Monster.Monster_Store, Monster_Pixel);
               end if; -- Image (X + 1) (Y) = '#'
            end loop; -- X in Mx
         end loop; -- Y in My
         Monster.Pixel_Count := Natural (Length (Monster.Monster_Store));
         return Monster;
      end Build_Monster;

      function Monster_Found
        (Picture : in Pictures.Images; Monster : in Monsters;
         X, Y : in Image_Coordinates) return Boolean
      is

         Result : Boolean := True;

      begin -- Monster_Found
         for M in Iterate (Monster.Monster_Store) loop
            Result :=
              Result and
              Picture
                (X + Monster.Monster_Store (M).X,
                 Y + Monster.Monster_Store (M).Y);
         end loop; -- M in Iterate (Monster.Monster_Store);
         return Result;
      end Monster_Found;

      procedure Put (Picture : in Pictures.Images) is

      begin -- Put
         Clear_Screen;
         for Y in Image_Coordinates loop
            Goto_XY (X_Pos'First, Y);
            for X in Image_Coordinates loop
               if Picture (X, Y) then
                  Put ('#');
               else
                  Put (' ');
               end if; -- Picture (X, Y)
            end loop; -- X in Image_Coordinates
         end loop; -- Y in Image_Coordinates
      end Put;

      procedure Put (Monster : in Monsters; X, Y : in Image_Coordinates) is

         Saved_Foreground : Color_Type := Get_Foreground;

      begin -- Put
         Set_Foreground (Green);
         for M in Iterate (Monster.Monster_Store) loop
            Goto_XY
              (X + Monster.Monster_Store (M).X,
               Y + Monster.Monster_Store (M).Y);
            Put ('O');
         end loop; -- M in Iterate (Monster.Monster_Store);
         Set_Foreground (Saved_Foreground);
      end Put;

      Saved_Picture : Pictures.Images := Assemble (Tile_Store, Side_Length);
      Picture : Pictures.Images;
      Monster : Monsters := Build_Monster;
      Monster_Count : Natural := 0;
      O : Pictures.Orientations := Pictures.Orientations'First;
      Picture_Pixels : Natural := 0;

   begin -- Part_Twos
      loop -- Search for Monsters
         Picture := Transform (Saved_Picture, O);
         Put (Picture);
         for X in Image_Coordinates range Image_Coordinates'First ..
             Image_Coordinates'Last - Monster.X_Limit
         loop
            for Y in Image_Coordinates range Image_Coordinates'First ..
                Image_Coordinates'Last - Monster.Y_Limit
            loop
               if Monster_Found (Picture, Monster, X, Y) then
                  Put (Monster, X, Y);
                  Monster_Count := Monster_Count + 1;
               end if; -- Monster_Found (Picture, Monster, X, Y)
            end loop; -- Y in Image_Coordinates range Image_Coordinates'First
         end loop; -- X in Image_Coordinates range Image_Coordinates'First ...
         exit when Monster_Count > 0 or O = Pictures.Orientations'Last;
         O := O + 1;
      end loop; -- Search for Monsters
      Goto_XY (Image_Coordinates'Last, Image_Coordinates'Last);
      New_Line;
      for X in Image_Coordinates loop
         for Y in Image_Coordinates loop
            if Picture (X, Y) then
               Picture_Pixels := Picture_Pixels + 1;
            end if; -- Picture (X, Y)
         end loop; -- Y in Image_Coordinates
      end loop; -- X in Image_Coordinate
      return Picture_Pixels - Monster.Pixel_Count * Monster_Count;
   end Part_Two;

   Tile_Store : Tile_Stores.Vector;
   Product : Tile_IDs := 1;
   Side_Length : Positive := 1;
   Remaining_Pixels : Natural := 0;

begin -- December_20
   Read_Tiles (Tile_Store);
   while Side_Length * Side_Length < Positive (Length (Tile_Store)) loop
      Side_Length := Side_Length + 1;
   end loop; -- Side_Length * Side_Length < Positive (Length (Tile_Store))
   Build_Trials (Tile_Store);
   Build_Matches (Tile_Store);
   for T in Iterate (Tile_Store) loop
      if Length (Tile_Store (T).Trial_Array (0).Match_list) = 2 then
         Product := Product * Tile_Store (T).Tile_ID;
      end if; -- Length (Tile_Store (T).Trial_Array (0).Match_list) = 2
   end loop; -- T in Iterate (Tile_Store)
   Remaining_Pixels := Part_Two (Tile_Store, Side_Length);
   Put_Line ("Part One:" & Product'Img);
   Put_Line ("Part Two:" & Remaining_Pixels'Img);
   Put_CPU_Time;
end December_20;
