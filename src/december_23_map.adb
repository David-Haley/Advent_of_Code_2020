with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Ordered_Maps;


procedure December_23_map is

   subtype Cup_Labels is Positive;

   Part_Two_Cups : constant Cup_Labels := 1000000;

   package Cups is new Ada.Containers.Ordered_Maps (Cup_Labels, Cup_Labels);
   use Cups;

   type Games is record
      Current_Cup : Cup_Labels;
      Cup : Cups.Map := Empty_Map;
      Pick_Up_First, Pick_Up_Last : Cup_Labels := Cup_Labels'First;
      Last_Label : Cup_Labels := Cup_Labels'First;
   end record; -- Games

   procedure Read_Game (Input_File : in out File_Type;
                        Game : out Games) is

      Ch : Character;
      First_Cup : Boolean := True;
      Cursor, Next_Cup : Cup_Labels;

   begin -- Read_Game
      Reset (Input_File);
      while not End_Of_Line (Input_File) loop
         Get (Input_File, Ch);
         Next_Cup := Character'Pos (Ch) - Character'Pos ('1') + 1;
         if First_Cup then
            First_Cup := False;
            Game.Current_Cup := Next_Cup;
            Cursor := Next_Cup;
         else
            Include (Game.Cup, Cursor, Next_Cup);
            Cursor := Next_Cup;
         end if; -- First_Cup
      end loop; -- not End_Of_Line (Input_File)
      -- Close circle
      Include (Game.Cup, Next_Cup, Game.Current_Cup);
      for I in Iterate (Game.Cup) loop
         if Key (I) > Game.Last_Label then
            Game.Last_Label := Key (I);
         end if; -- Key (I) > Game.Last_Label
      end loop; -- I in Iterate (Game.Cup)
   end Read_Game;

   procedure Play (Game : in out Games; Moves : in Positive) is

      function Dec (Game : in Games;
                    Cup_Label : in Cup_Labels) return Cup_Labels is

      begin -- Dec
         if Cup_Label = Cup_Labels'First then
            return Game.Last_Label;
         else
            return Cup_Labels'Pred (Cup_Label);
         end if; -- Cup_Label = Cup_Labels'First
      end Dec;

      function In_Pick_Up (Game : in Games;
                           Label : in Cup_Labels) return Boolean is

      begin -- In_Pick_Up
         return Label = Game.Pick_Up_First or
           Label = Game.Cup (Game.Pick_Up_First) or
           Label = Game.Pick_Up_Last;
      end In_Pick_Up;

      Cursor, Destination : Cup_Labels;

   begin -- Play
      for Move in Positive range 1 .. Moves loop
         -- pickup three cups
         Cursor := Game.Cup (Game.Current_Cup); -- 1
         Game.Pick_Up_First := Cursor;
         Cursor := Game.Cup (Cursor); -- 2
         Game.Pick_Up_Last := Game.Cup (Cursor); -- 3
         -- close cup circle
         Game.Cup (Game.Current_Cup) := Game.Cup (Game.Pick_Up_Last);
         -- select destination
         Destination := Dec (Game, Game.Current_Cup);
         while In_Pick_Up (Game, Destination) loop
            Destination := Dec (Game, Destination);
         end loop; -- In_Pick_Up (Game, Destination)
         -- Insert Pickup
         Game.Cup (Game.Pick_Up_Last) := Game.Cup (Destination);
         Game.Cup (Destination) := Game.Pick_Up_First;
         Game.Current_Cup := Game.Cup (Game.Current_Cup);
      end loop; -- Moves in Positive range 1 .. Moves
   end Play;

   Input_File : File_Type;
   Game : Games; --Game state variable
   Cursor : Cup_Labels;
   Star_1, Star_2 : Cup_Labels;

begin -- December_23_map
   Open (Input_File, In_File, "december_23.txt");
   -- Open (Input_File, In_File, "example_23.txt");
   Read_Game (Input_File, Game);
   Play (Game, 100);
   Put ("Sequence (Part one): ");
   Cursor := Game.Cup (1);
   while Cursor /= 1 loop
      Put (Trim (Cup_Labels'Image (Cursor), Both));
      Cursor := Game.Cup (Cursor);
   end loop; -- Cursor.Cup_Label /= '1'
   New_Line;
   -- Part two
   Read_Game (Input_File, Game);
   Close (Input_File);
   Cursor := Game.Current_Cup;
   while Game.Cup (Cursor) /= Game.Current_Cup loop
      Cursor := Game.Cup (Cursor);
   end loop; -- Cursor.Next_Cup /= Game.Current_Cup
   Game.Cup (Cursor) := Game.Last_Label + 1; -- Link to first additional cup
   for I in Cup_Labels range Game.Last_Label + 1 .. Part_Two_Cups loop
      Include (Game.Cup, I, I + 1);
   end loop; -- I in Cup_Labels range Game.Last_Label + 1 .. Part_Two_Cups
   Game.Cup (Part_Two_Cups) := Game.Current_Cup; -- Close circle
   Game.Last_Label := Part_Two_Cups; -- limit of additional cups
   Play (Game, 10000000);
   Star_1 := Game.Cup (1); -- Cup next to 1
   Star_2 := Game.Cup (Star_1);
   Put_Line ("Product (Part two):" &
               Long_Long_Integer'Image (Long_Long_Integer (Star_1) *
                 Long_Long_Integer (Star_2)));
end December_23_map;
