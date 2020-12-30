with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure December_22 is

   subtype Player_Indices is Boolean;
   P1 : Player_Indices := Player_Indices'First;
   P2 : Player_Indices := Player_Indices'Last;

   subtype Cards is Positive;
   type Drawn_Cards is array (Player_Indices) of Cards;

   package Player_Decks is new Ada.Containers.Vectors (Positive, Cards);
   use Player_Decks;

   type Decks is array (Player_Indices) of Player_Decks.Vector;

   type Games is record
      Last_Winner : Player_Indices;
      Drawn_Card : Drawn_Cards;
      Deck : Decks;
   end record; -- Games

   procedure Start_Game (Input_File : in out File_Type; Game : out Games) is

      Player_String : constant String := "Player";
      Current_Player : Player_Indices;
      Text : Unbounded_String;
      First : Positive;
      Last : Natural;
      Card : Cards;

   begin -- Start_Game
      Reset (Input_File);
      Game.Last_Winner := P1;
      Game.Drawn_Card := (others => Cards'First);
      Game.Deck := (others => Player_Decks.Empty_Vector);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         if Index (Text, Player_String) > 0 then
            Find_Token (Text, Decimal_Digit_Set, Inside, First, Last);
            if Element (Text, First) = '1' then
               Current_Player := P1;
            elsif Element (Text, First) = '2' then
               Current_Player := P2;
            else
               Assert (False, "Bad Player '" & Element (Text, First) & "'");
            end if; -- Number = 1
         elsif Length (Text) > 0 then
            Card := Cards'Value (To_String (Text));
            Append (Game.Deck (Current_Player), Card);
         end if; -- Index (Text, "Player") > 0
      end loop; -- not End_Of_File (Input_File)
   end Start_Game;

   procedure Play_One (Game : in out Games) is

   begin -- Play_One
      while Length (Game.Deck (P1)) > 0 and Length (Game.Deck (P2)) > 0 loop
         for P in Player_Indices loop
            Game.Drawn_Card (P) := First_Element (Game.Deck (P));
            Delete_First (Game.Deck (P));
         end loop; -- P in Player_Indices
         if Game.Drawn_Card (P1) > Game.Drawn_Card (P2) then
            Game.Last_Winner := P1;
         else
            Game.Last_Winner := P2;
         end if; -- Game.Drawn_Card (P1) > Game.Drawn_Card (P2)
         Append (Game.Deck (Game.Last_Winner),
                 Game.Drawn_Card (Game.Last_Winner));
         Append (Game.Deck (Game.Last_Winner),
                 Game.Drawn_Card (not Game.Last_Winner));
      end loop; -- Length (Game.Deck (P1)) > 0 and Length (Game.Deck (P2)) > 0
   end Play_One;

   procedure Play_Two (Game : in out Games) is

      package Deck_Sets is new Ada.Containers.Ordered_Sets (Unbounded_String);
      use Deck_Sets;

      function Deck_To_String (Game : in Games) return Unbounded_String is

         Result : Unbounded_String := Null_Unbounded_String;

      begin -- Deck_To_String
         for P in Player_Indices loop
            Append (Result, P'Img);
         for C in Iterate (Game.Deck (P)) loop
            Append (Result, Cards'Image (Game.Deck (P) (C)));
            end loop; -- C in Iterate (Game.Deck (P))
         end loop; -- P in Player_Indices
         return Result;
      end Deck_To_String;

      function Player_One_Win (Game : in Games;
                               Deck_Set : in Deck_Sets.Set) return Boolean is

      begin -- Player_One_Win
         return Contains (Deck_Set, Deck_To_String (Game));
      end Player_One_Win;

      Next_Game : Games;
      Deck_Set : Deck_Sets.Set := Deck_Sets.Empty_Set;
      Round : Positive := 1;

   begin -- Play_Two
      while Length (Game.Deck (P1)) > 0 and Length (Game.Deck (P2)) > 0 loop
         Round := Round + 1;
         if Player_One_Win (Game, Deck_Set) then
            Game.Last_Winner := P1;
            exit;
            -- Inelegant exit, it is assumed that this will only occur in a sub
            -- game, where the result will be resolved in the game above.
         else
            Include (Deck_Set, Deck_To_String (Game));
            for P in Player_Indices loop
               Game.Drawn_Card (P) := First_Element (Game.Deck (P));
               Delete_First (Game.Deck (P));
            end loop; -- P in Player_Indices
            if Cards (Length (Game.Deck (P1))) >= Game.Drawn_Card (P1) and
              Cards (Length (Game.Deck (P2))) >= Game.Drawn_Card (P2) then
               -- Start recursive play, by copying cards
               for P in Player_Indices loop
                  Clear (Next_Game.Deck (P));
                  for I in Positive range 1 .. Game.Drawn_Card (P) loop
                     Append (Next_Game.Deck (P), Game.Deck (P) (I));
                  end loop; -- I in Positive range 1 .. Game.Drawn_Card (P)
               end loop; -- P in Player_Indices
               Play_Two (Next_Game);
               -- only part of the returned game that matters was the winner
               Game.Last_Winner := Next_Game.Last_Winner;
            else
               if Game.Drawn_Card (P1) > Game.Drawn_Card (P2) then
                  Game.Last_Winner := P1;
               else
                  Game.Last_Winner := P2;
               end if; -- Game.Drawn_Card (P1) > Game.Drawn_Card (P2)
            end if; -- Cards (Length (Game.Deck (P1))) >= Game.Drawn_Card  ...
               Append (Game.Deck (Game.Last_Winner),
                       Game.Drawn_Card (Game.Last_Winner));
               Append (Game.Deck (Game.Last_Winner),
                       Game.Drawn_Card (not Game.Last_Winner));
         end if; -- Player_One_Win
      end loop; -- Length (Game.Deck (P1)) > 0 and Length (Game.Deck (P2)) > 0
   end Play_Two;

   function Score (Game : in Games) return Natural is

      Multiplier : Positive := 1;
      Sum : Natural := 0;

   begin -- Score
      for C in reverse Iterate (Game.Deck (Game.Last_Winner)) loop
         Sum := Sum + Element (C) * Multiplier;
         Multiplier := Multiplier + 1;
      end loop; -- C in reverse Iterate (Game.Deck (Game.Last_Winner))
      return Sum;
   end; -- Score

   Input_File : File_Type;
   Game : Games; --Game state variable

begin -- December_22
   Open (Input_File, In_File, "december_22.txt");
   Start_Game (Input_File, Game);
   Play_One (Game);
   Put_Line ("Score (Part one)" & Score (Game)'Img);
   Start_Game (Input_File, Game);
   Play_Two (Game);
   Put_Line ("Score (Part two)" & Score (Game)'Img);
   Close (Input_File);
end December_22;
