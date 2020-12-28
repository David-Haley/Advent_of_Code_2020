with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure December_23 is

   subtype Cup_Labels is Long_Long_Integer range 1 .. Long_Long_Integer'Last;

   type Cup_Nodes;
   type Cup_Pointers is access Cup_nodes;
   type Cup_Nodes is record
      Cup_Label : Cup_Labels;
      Next_Cup : Cup_Pointers := null;
   end record; -- Cup_Nodes

   type Games is record
      Current_Cup : Cup_Pointers := null;
      Pick_Up : Cup_Pointers := null;
      Pick_Up_Last : Cup_Pointers := null;
      Last_Label : Cup_Labels := Cup_Labels'First;
   end record; -- Games

   procedure Put (Game : in Games) is

      Cursor : Cup_Pointers := Game.Current_Cup;

   begin -- Put
      Put_Line ("Current Cup:" & Game.Current_Cup.Cup_Label'Img);
      Put ("Cups:");
      loop
         Put (Cursor.Cup_Label'Img);
         Cursor := Cursor.Next_Cup;
         exit when Game.Current_Cup = Cursor;
      end loop;
      New_Line;
      Put ("Pick_Up:");
      Cursor := Game.Pick_Up;
      while Cursor /= null loop
         Put (Cursor.Cup_Label'Img);
         Cursor := Cursor.Next_Cup;
      end loop; -- Cursor /= null
      New_Line;
      Put ("Pick_Up_Last:");
      if Game.Pick_Up_Last /= null then
         Put (Game.Pick_Up_Last.Cup_Label'Img);
      end if;
      New_Line;
      Put_Line ("Last Label" & Game.Last_Label'Img);
   end Put;

   procedure Read_Game (Input_File : in out File_Type;
                        Game : out Games) is

      Ch : Character;
      First_Cup : Boolean := True;
      Cursor : Cup_Pointers;

   begin -- Read_Game
      Reset (Input_File);
      while not End_Of_Line (Input_File) loop
         if First_Cup then
            First_Cup := False;
            Game.Current_Cup := new Cup_Nodes;
            Cursor := Game.Current_Cup;
         else
            Cursor.Next_Cup := new Cup_Nodes;
            Cursor := Cursor.Next_Cup;
         end if; -- First_Cup
         Get (Input_File, Ch);
         Cursor.Cup_Label := Character'Pos (Ch) -
           Character'Pos ('1') + 1;
         if Cursor.Cup_Label > Game.Last_Label then
            Game.Last_Label := Cursor.Cup_Label;
         end if; -- Cursor.Cup_Label > Game.Last_Label
      end loop; -- not End_Of_Line (Input_File)
      Cursor.Next_Cup := Game.Current_Cup;
      -- Close circle
   end Read_Game;

   function Dec (Game : in Games;
                 Cup_Label : in Cup_Labels) return Cup_Labels is

   begin -- Dec
      if Cup_Label = Cup_Labels'First then
         return Game.Last_Label;
      else
         return Cup_Labels'Pred (Cup_Label);
      end if; -- Cup_Position = Cup_Positions'First
   end Dec;

   function Find_Cup (Game : in Games;
                      Cup_Label : in Cup_Labels) return Cup_Pointers is

      -- assunes Cup_Label is in Game.Current_Cup

      Cursor : Cup_Pointers := Game.Current_Cup;

   begin -- Find_Cup
      while Cursor.Cup_Label /= Cup_Label loop
         Cursor := Cursor.Next_Cup;
      end loop; --  Cursor.Next_Cup /= Game.Current_Cup and ...
      return Cursor;
   end Find_Cup;

   function In_Pick_Up (Game : in Games;
                        Cup_Label : in Cup_Labels) return Boolean is

      Cursor : Cup_Pointers := Game.Pick_Up;
      Found : Boolean := False;

   begin -- In_Pick_Up
      while Cursor /= null and not Found loop
         Found := Cursor.Cup_Label = Cup_Label;
         Cursor := Cursor.Next_Cup;
      end loop; -- Cursor /= null and not Found
      return Found;
   end In_Pick_Up;

   procedure Play (Game : in out Games; Moves : in Positive) is

      Destination_Label : Cup_Labels;
      Cursor, Destination : Cup_Pointers;

   begin -- Play
      for Move in Positive range 1 .. Moves loop
         -- pickup three cups
         Cursor := Game.Current_Cup.Next_Cup; -- 1
         Game.Pick_Up := Cursor;
         Cursor := Cursor.Next_Cup; -- 2
         Cursor := Cursor.Next_Cup; -- 3
         Game.Pick_Up_Last := Cursor;
         Cursor := Cursor.Next_Cup;
         Game.Current_Cup.Next_Cup := Cursor; -- close circle
         Game.Pick_Up_Last.Next_Cup := null; -- unlink
         -- select destination
         Destination_Label := Dec (Game, Game.Current_Cup.Cup_Label);
         while In_Pick_Up (Game, Destination_Label) loop
            Destination_Label := Dec (Game, Destination_Label);
         end loop; -- Find_Cup (Game, Destination) = null
         Destination := Find_Cup (Game, Destination_Label);
         Cursor := Destination.Next_Cup;
         Destination.Next_Cup := Game.Pick_Up; -- link start of pickup in
         Game.Pick_Up_Last.Next_Cup := Cursor;
         Game.Pick_Up := null;
         Game.Pick_Up_Last := null;
         Game.Current_Cup := Game.Current_Cup.Next_Cup;
         if Move mod 10000 = 0 then
            Put_Line ("Move:" & Move'Image);
         end if; -- Move mod 100000 = 0
      end loop; -- Moves in Positive range 1 .. Moves
   end Play;

   Input_File : File_Type;
   Game : Games; --Game state variable
   Cursor : Cup_Pointers;
   Star_1, Star_2 : Cup_Labels;

begin -- December_23
   Open (Input_File, In_File, "december_23.txt");
   Read_Game (Input_File, Game);
   Play (Game, 100);
   Put ("Sequence (Part one): ");
   Cursor := Find_Cup (Game, 1);
   Cursor := Cursor.Next_Cup;
   while Cursor.Cup_Label /= 1 loop
      Put (Trim (Cup_Labels'Image (Cursor.Cup_Label), Both));
      Cursor := Cursor.Next_Cup;
   end loop; -- Cursor.Cup_Label /= '1'
   New_Line;
   -- Part two
   Read_Game (Input_File, Game);
   Cursor := Game.Current_Cup;
   while Cursor.Next_Cup /= Game.Current_Cup loop
      Cursor := Cursor.Next_Cup;
   end loop; -- Cursor.Next_Cup /= Game.Current_Cup
   for I in Cup_Labels range Game.Last_Label + 1 .. 1000000 loop
      Cursor.Next_Cup := new Cup_Nodes;
      Cursor := Cursor.Next_Cup;
      Cursor.Cup_Label := I;
      Game.Last_Label := I;
   end loop; -- I in Cup_Labels range Game.Last_Label + 1 .. 1000000
   Cursor.Next_Cup := Game.Current_Cup;
   Play (Game, 10000000);
   Cursor := Find_Cup (Game, 1);
   Cursor := Cursor.Next_Cup;
   Star_1 := Cursor.Cup_Label;
   Cursor := Cursor.Next_Cup;
   Star_2 := Cursor.Cup_Label;
   Put_Line (Star_1'img & Star_2'Img);
   Put_Line ("Product (Part two):" & Cup_Labels'Image (Star_1 * Star_2));
end December_23;
