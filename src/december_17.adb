with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_17 is

   subtype Cycles is Positive range 1 .. 6;

   procedure Solve (Input_File : in out File_Type;
                    X_Limit, Y_limit : in Natural) is

      subtype X_Coordinates is Integer
      range 1 - Cycles'Last .. X_Limit + Cycles'Last;
      subtype Y_Coordinates is Integer
      range 1 - Cycles'Last .. Y_Limit + Cycles'Last;
      subtype Z_Coordinates is Integer range -Cycles'Last .. Cycles'Last;

      type Cube_Spaces is array (X_Coordinates, Y_Coordinates, Z_Coordinates) of
        Boolean;

      procedure Read_Input (Input_File : in out File_Type;
                            X_Limit, Y_limit : in Natural;
                            Cube_Space : out Cube_Spaces) is

         Ch : Character;

      begin -- Read_Input
         Reset (Input_File);
         Cube_Space := (others => (others => (others => False)));
         for Y in Positive range 1 .. Y_Limit loop
            for X in Positive range 1 .. X_Limit loop
               Get (Input_File, Ch);
               if Ch = '#' then
                  Cube_Space (X, Y, 0) := True;
               else
                  Assert (Ch = '.', "Expected '.' and found '" & Ch &
                            "' at line:" & Positive'Image (Y));
               end if; --  Ch = '#'
            end loop; -- X in Positive range 1 .. X_Limit
         end loop; -- Y in Positive range 1 .. Y_Limit
      end Read_Input;

      procedure Generate (Cube_Space : in Cube_Spaces;
                          Next_Cube_Space : out Cube_Spaces) is

         function Count_Active (X : in X_Coordinates;
                                Y : in Y_Coordinates;
                                Z : in Z_Coordinates;
                                Cube_Space : in Cube_Spaces) return Natural is

            subtype Offsets is Integer range -1 .. 1;

            Count : Natural := 0;

         begin -- Count_Active
            for Xo in Offsets loop
               for Yo in Offsets loop
                  for Zo in Offsets Loop
                     if (X + Xo in X_Coordinates and
                           Y + Yo in Y_Coordinates and
                             Z + Zo in Z_Coordinates and
                           (Xo /= 0 or Yo /= 0 or Zo /= 0)) and then
                       Cube_Space (X + Xo, Y + Yo, Z + Zo) then
                        Count := Count + 1;
                     end if; -- (X + Xo in X_Coordinates and ...
                  end loop; -- Zo in Offsets
               end loop; -- Yo in Offsets
            end loop; -- Xo in Offsets
            return Count;
         end Count_Active;

         Count : Natural;
         -- temp variable used to avoid multiple calls to Count_Active
         -- Also could be spead up by increasing coordinate limits on each
         -- cycle.

      begin -- Generate
         for X in X_Coordinates loop
            for Y in Y_Coordinates loop
               for Z in Z_Coordinates loop
                  Count := Count_Active (X, Y, Z, Cube_Space);
                  if Cube_Space (X, Y, Z) then
                     if Count = 2 or Count = 3 then
                        Next_Cube_Space (X, Y, Z) := True;
                     else
                        Next_Cube_Space (X, Y, Z) := False;
                     end if; -- Count = 2 or Count = 3
                  else
                     if Count = 3 then
                        Next_Cube_Space (X, Y, Z) := True;
                     else
                        Next_Cube_Space (X, Y, Z) := False;
                     end if; -- Count = 3
                  end if; -- Cube_Space (X, Y, Z)
               end loop; -- Z in Z_Coordinates
            end loop; -- Y in Y_Coordinates
         end loop; -- X in X_Coordinates
      end Generate;

      function Count_Active (Cube_Space : in Cube_Spaces) return Natural is

         Count : Natural := 0;

      begin -- Count_Active
         for X in X_Coordinates loop
            for Y in Y_Coordinates loop
               for Z in Z_Coordinates Loop
                  if Cube_Space (X, Y, Z) then
                     Count := Count + 1;
                  end if; -- Cube_Space (X, Y, Z)
               end loop; -- Z in Z_Coordinates
            end loop; -- Y in Y_Coordinates
         end loop; -- X in X_Coordinates
         return Count;
      end Count_Active;

      Cube_Space, Next_Cube_Space : Cube_Spaces;

   begin -- Solve
      Read_Input (Input_File, X_Limit, Y_Limit, Cube_Space);
      for C in Cycles loop
         Generate (Cube_Space, Next_Cube_Space);
         Cube_Space := Next_Cube_Space;
      end loop;
      Put_Line ("Active count (Part one)" &
                  Natural'Image (Count_Active (Cube_Space)));
   end Solve;

   procedure Solve_2 (Input_File : in out File_Type;
                      X_Limit, Y_limit : in Natural) is

      subtype X_Coordinates is Integer
      range 1 - Cycles'Last .. X_Limit + Cycles'Last;
      subtype Y_Coordinates is Integer
      range 1 - Cycles'Last .. Y_Limit + Cycles'Last;
      subtype Z_Coordinates is Integer range -Cycles'Last .. Cycles'Last;
      subtype W_Coordinates is Integer range -Cycles'Last .. Cycles'Last;

      type Cube_Spaces is array (X_Coordinates, Y_Coordinates,
                                 Z_Coordinates, W_Coordinates) of Boolean;

      procedure Read_Input (Input_File : in out File_Type;
                            X_Limit, Y_limit : in Natural;
                            Cube_Space : out Cube_Spaces) is

         Ch : Character;

      begin -- Read_Input
         Reset (Input_File);
         Cube_Space := (others => (others => ( others => (others => False))));
         for Y in Positive range 1 .. Y_Limit loop
            for X in Positive range 1 .. X_Limit loop
               Get (Input_File, Ch);
               if Ch = '#' then
                  Cube_Space (X, Y, 0, 0) := True;
               else
                  Assert (Ch = '.', "Expected '.' and found '" & Ch &
                            "' at line:" & Positive'Image (Y));
               end if; --  Ch = '#'
            end loop; -- X in Positive range 1 .. X_Limit
         end loop; -- Y in Positive range 1 .. Y_Limit
      end Read_Input;

      procedure Generate (Cube_Space : in Cube_Spaces;
                          Next_Cube_Space : out Cube_Spaces) is

         function Count_Active (X : in X_Coordinates;
                                Y : in Y_Coordinates;
                                Z : in Z_Coordinates;
                                W : in W_Coordinates;
                                Cube_Space : in Cube_Spaces) return Natural is

            subtype Offsets is Integer range -1 .. 1;

            Count : Natural := 0;

         begin -- Count_Active
            for Xo in Offsets loop
               for Yo in Offsets loop
                  for Zo in Offsets Loop
                     for Wo in Offsets loop
                        if (X + Xo in X_Coordinates and
                              Y + Yo in Y_Coordinates and
                                Z + Zo in Z_Coordinates and
                                  W + Wo in W_Coordinates and
                              (Xo /= 0 or Yo /= 0 or Zo /= 0 or Wo /= 0))
                          and then
                          Cube_Space (X + Xo, Y + Yo, Z + Zo, W + Wo) then
                           Count := Count + 1;
                        end if; -- (X + Xo in X_Coordinates and ...
                     end loop; -- Wo in Offsets
                  end loop; -- Zo in Offsets
               end loop; -- Yo in Offsets
            end loop; -- Xo in Offsets
            return Count;
         end Count_Active;

         Count : Natural;
         -- temp variable used to avoid multiple calls to Count_Active
         -- Also could be spead up by increasing coordinate limits on each
         -- cycle.

      begin -- Generate
         for X in X_Coordinates loop
            for Y in Y_Coordinates loop
               for Z in Z_Coordinates loop
                  for W in W_Coordinates loop
                     Count := Count_Active (X, Y, Z, W, Cube_Space);
                     if Cube_Space (X, Y, Z, W) then
                        if Count = 2 or Count = 3 then
                           Next_Cube_Space (X, Y, Z, W) := True;
                        else
                           Next_Cube_Space (X, Y, Z, W) := False;
                        end if; -- Count = 2 or Count = 3
                     else
                        if Count = 3 then
                           Next_Cube_Space (X, Y, Z, W) := True;
                        else
                           Next_Cube_Space (X, Y, Z, W) := False;
                        end if; -- Count = 3
                     end if; -- Cube_Space (X, Y, Z, W)
                  end loop; -- W in W_Coordinates
               end loop; -- Z in Z_Coordinates
            end loop; -- Y in Y_Coordinates
         end loop; -- X in X_Coordinates
      end Generate;

      function Count_Active (Cube_Space : in Cube_Spaces) return Natural is

         Count : Natural := 0;

      begin -- Count_Active
         for X in X_Coordinates loop
            for Y in Y_Coordinates loop
               for Z in Z_Coordinates Loop
                  for W in W_Coordinates loop
                     if Cube_Space (X, Y, Z, W) then
                        Count := Count + 1;
                     end if; -- Cube_Space (X, Y, Z, W)
                  end loop; --  W in W_Coordinates
               end loop; -- Z in Z_Coordinates
            end loop; -- Y in Y_Coordinates
         end loop; -- X in X_Coordinates
         return Count;
      end Count_Active;

      Cube_Space, Next_Cube_Space : Cube_Spaces;

   begin -- Solve_2
      Read_Input (Input_File, X_Limit, Y_Limit, Cube_Space);
      for C in Cycles loop
         Generate (Cube_Space, Next_Cube_Space);
         Cube_Space := Next_Cube_Space;
      end loop;
      Put_Line ("Active count (Part two)" &
                  Natural'Image (Count_Active (Cube_Space)));
   end Solve_2;

   Input_File : File_Type;
   Text : Unbounded_String;
   X_limit, Y_Limit : Natural := 0;

begin -- December_17
   Open (Input_File, In_File, "december_17.txt");
      while not End_Of_Line (Input_File) loop
         Get_Line (Input_File, Text);
         if Y_Limit = 0 then
            X_Limit := Length (Text);
         else
            Assert (X_limit = Length (Text), "lines not the same length");
         end if; -- Y_Limit = 0
         Y_Limit := Y_Limit + 1;
   end loop; -- not End_Of_Line (Input_File)
   Solve (Input_File, X_Limit, Y_Limit);
   Solve_2 (Input_File, X_Limit, Y_Limit);
   Close (Input_File);
end December_17;
