with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_11 is

   procedure Solve (Input_File : in File_Type;
                    X_Limit, Y_Limit : in Natural) is

      subtype X_Coordinates is Natural range 0 .. X_Limit - 1;
      subtype Y_Coordinates is Natural range 0 .. Y_Limit - 1;
      Floor : constant Character := '.';
      Seat : constant Character := 'L';
      Occupied : constant Character := '#';
      subtype Deck_Elements is Character with
        Static_Predicate => Deck_Elements in Floor | Seat | Occupied;
      type Decks is array (X_Coordinates, Y_Coordinates) of Deck_Elements;

      function Count_Occupied (Deck : in Decks) return Natural is

         Result : Natural := 0;

      begin -- Count_Occupied
         for X in X_Coordinates loop
            for Y in Y_Coordinates loop
               if Deck (X, Y) = Occupied then
                  Result := Result + 1;
               end if; -- Deck (X, Y) = Occupied
            end loop; -- Y in Y_Coordinates
         end loop; -- X in X_Coordinates
         return Result;
      end Count_Occupied;

      procedure Animate_One (Deck_In : in Decks) is

         function Next_State (Deck : in Decks;
                              X_In : in X_Coordinates;
                              Y_in : in Y_Coordinates) return Deck_Elements is

            subtype Counts is Natural range 0 .. 8;

            Occupied_Seats : Counts := 0;
            X_Min, X_Max : X_Coordinates;
            Y_Min, Y_Max : Y_Coordinates;

         begin -- Next_State
            if Deck (X_in, Y_in) = Floor then
               return Floor;
               -- Floor always unchanged
            else
               if X_In - 1 in X_Coordinates then
                  X_Min := X_In - 1;
               else
                  X_Min := X_In;
               end if; -- X_In - 1 in X_Coordinates
               if X_In + 1 in X_Coordinates then
                  X_Max := X_In + 1;
               else
                  X_Max := X_In;
               end if; -- X_In + 1 in X_Coordinates
               if Y_In - 1 in Y_Coordinates then
                  Y_Min := Y_In - 1;
               else
                  Y_Min := Y_In;
               end if; -- Y_In - 1 in Y_Coordinates
               if Y_In + 1 in Y_Coordinates then
                  Y_Max := Y_In + 1;
               else
                  Y_Max := Y_In;
               end if; -- Y_In + 1 in Y_Coordinates
               for X in X_Coordinates range X_Min .. X_Max loop
                  for Y in Y_Coordinates range Y_Min .. Y_Max loop
                     if (X /= X_In or Y /= Y_In) and then
                       Deck (X, Y) = Occupied then
                        Occupied_Seats := Occupied_Seats + 1;
                     end if; -- (X /= X_In or Y /= Y_In) and then ...
                  end loop; -- Y in Y_Coordinates range Y_Min .. Y_Max
               end loop; -- X in X_Coordinates range X_Min .. X_Max
               if Deck (X_In, Y_in) = Seat and Occupied_Seats = 0 then
                  return Occupied;
               elsif Deck (X_In, Y_in) = Occupied and Occupied_Seats >= 4 then
                  return Seat;
               else
                  return Deck (X_In, Y_in);
               end if; -- Deck (X_In, Y_in) = Seat and Occupied_Seats = 0
            end if; -- Deck (X_in, Y_in) = Floor
         end Next_State;

         Deck, Previous_Deck : Decks := Deck_In;

      begin -- Animate_One
         loop -- one sweep of deck
            Previous_Deck := Deck;
            for X in X_Coordinates loop
               for Y in Y_Coordinates loop
                  Deck (X, Y) := Next_State (Previous_Deck, X, Y);
               end loop; -- Y in Y_Coordinates
            end loop; -- X in X_Coordinates
            exit when Deck = Previous_Deck;
         end loop; -- one sweep of deck
         Put_Line ("Occupied seats (part one):" &
                     Natural'Image (Count_Occupied (Deck)));
      end Animate_One;

      procedure Animate_Two (Deck_In : in Decks) is

         function Next_State (Deck : in Decks;
                              X : in X_Coordinates;
                              Y : in Y_Coordinates) return Deck_Elements is

            type Directions is (N, NE, E, SE, S, SW, W, NW);

            function Can_See_Occupied (Deck : in Decks;
                                       X_In : in X_Coordinates;
                                       Y_In : in Y_Coordinates;
                                       Direction : in Directions)
                                       return Boolean is

               subtype Increments is Integer range -1 .. 1;
               type Steps is record
                  Xs, Ys : Increments;
               end record; -- Steps
               Step : constant array (Directions) of Steps :=
                 (N  => ( 0, -1),
                  NE => ( 1, -1),
                  E  => ( 1,  0),
                  SE => ( 1,  1),
                  S  => ( 0,  1),
                  SW => (-1,  1),
                  W  => (-1,  0),
                  NW => (-1, -1));

               X : Integer := X_In;
               Y : Integer := Y_in;

            begin -- Can_See_Occupied
               loop -- one step in Direction
                  X := X + Step (Direction).Xs;
                  Y := Y + Step (Direction).Ys;
                  if X in X_Coordinates and Y in Y_Coordinates then
                     if Deck (X, Y) = Seat then
                        return False;
                     elsif Deck (X, Y) = Occupied then
                        return True;
                     end if; -- Deck (X, Y) = Seat
                  else
                     return False; -- bounds of Deck exceded
                  end if; -- X in X_Coordinates and Y in Y_Coordinates
               end loop; -- one step in Direction
            end Can_See_Occupied;

            subtype Counts is Natural range 0 .. 8;

            Occupied_Seats : Counts := 0;

         begin -- Next_State
            if Deck (X, Y) = Floor then
               return Floor;
               -- Floor always unchanged
            else
               for D in Directions loop
                  if Can_See_Occupied (Deck, X, Y, D) then
                     Occupied_Seats := Occupied_Seats + 1;
                  end if; -- Can_See_Occupied (Deck, X, Y, D)
               end loop; -- D in Directions
               if Deck (X, Y) = Seat and Occupied_Seats = 0 then
                  return Occupied;
               elsif Deck (X, Y) = Occupied and Occupied_Seats >= 5 then
                  return Seat;
               else
                  return Deck (X, Y);
               end if; -- Deck (X, Y) = Seat and Occupied_Seats = 0
            end if; -- Deck (X, Y) = Floor
         end Next_State;

         Deck, Previous_Deck : Decks := Deck_In;

      begin -- Animate_Two
         loop -- one sweep of deck
            Previous_Deck := Deck;
            for X in X_Coordinates loop
               for Y in Y_Coordinates loop
                  Deck (X, Y) := Next_State (Previous_Deck, X, Y);
               end loop; -- Y in Y_Coordinates
            end loop; -- X in X_Coordinates
            exit when Deck = Previous_Deck;
         end loop; -- one sweep of deck
         Put_Line ("Occupied seats (part two):" &
                     Natural'Image (Count_Occupied (Deck)));
      end Animate_Two;

      Deck : Decks;

   begin -- Solve
      for Y in Y_Coordinates loop
         for X in X_Coordinates loop
            Get (Input_File, Deck (X, Y));
         end loop; -- X in X_Coordinates
         Skip_Line (Input_File);
      end loop; -- Y in Y_Coordinates
      Animate_One (Deck);
      Animate_Two (Deck);
   end Solve;

   Input_File : File_Type;
   Text : Unbounded_String;
   X_Limit, Y_Limit : Natural := 0;

begin -- December_11
   Open (Input_File, In_File, "december_11.txt");
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
end December_11;
