with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;

procedure December_12 is

   North : constant Character := 'N';
   South : constant Character := 'S';
   East : constant Character := 'E';
   West : constant Character := 'W';
   Forward : constant Character := 'F';
   Left : constant Character := 'L';
   Right : constant Character := 'R';

   subtype Directions is Character with Static_Predicate => Directions in
     North | South | East | West | Forward | Left | Right;

   subtype Headings is Directions with Static_Predicate => Headings in
     North | South | East | West;

   subtype Turns is Directions with Static_Predicate => Turns in Left | Right;

   subtype Coordinates is Integer;

   type Nav_States is record
      X, Y : Coordinates;
      Heading : Headings; -- not used in part 2
      Way_X, Way_Y : Coordinates; -- relative to ship's position, part two only
   end record; -- Nav_States

   subtype Steps is Positive;

   package Step_IO is new Ada.Text_IO.Integer_IO (Steps);

   Type Angles is mod 360 with
     Static_Predicate => Angles in 0 | 90 | 180 | 270;
   -- 0 may not exist in input data but is necessary for modular angle
   -- arithmetic in part one;

   package Angle_IO is new Ada.Text_IO.Modular_IO (Angles);

   type Instructions (Direction : Directions) is record
      case Direction is
         when North | South | East | West | Forward =>
            Step : Steps;
         when Left | Right =>
            Angle : Angles;
      end case; -- Direction
   end record; -- Instructions

   Package Instruction_Lists is new
     Ada.Containers.Indefinite_Vectors (Positive, Instructions);
   -- Must be indefinite because of variant record.
   use Instruction_Lists;

   function Move (Nav_State : in Nav_States;
                  Instruction : in Instructions) return Nav_States is

      function New_Heading (Heading : in Headings;
                            Turn : in Turns;
                            Angle : in Angles) return Headings is

         Result : Headings;
         New_Angle : Angles;

      begin -- New_Heading
         case Heading is
         when North => New_Angle := 0;
         when East => New_Angle := 90;
         when South => New_Angle := 180;
         when West => New_Angle := 270;
         end case; -- Heading
         case Turn is
         when Left =>
            New_Angle := New_Angle - Angle;
         when Right =>
            New_Angle := New_Angle + Angle;
         end case; -- Turn
         case New_Angle is
         when 0 => Result := North;
         when 90 => Result := East;
         when 180 => Result := South;
         when 270 => Result := West;
         end case; -- New_Angle
         return Result;
      end New_Heading;

      New_Nav_State : Nav_States := Nav_State;

   begin -- Move
      case Instruction.Direction is
         when North =>
            New_Nav_State.Y := Nav_State.Y + Instruction.Step;
         when South =>
            New_Nav_State.Y := Nav_State.Y - Instruction.Step;
         when East =>
            New_Nav_State.X := Nav_State.X + Instruction.Step;
         when West =>
            New_Nav_State.X := Nav_State.X - Instruction.Step;
         when Left | Right =>
            New_Nav_State.Heading := New_Heading (Nav_State.Heading,
                                                  Instruction.Direction,
                                                  Instruction.Angle);
         when Forward =>
            case Nav_State.Heading is
            when North =>
               New_Nav_State.Y := Nav_State.Y + Instruction.Step;
            when South =>
               New_Nav_State.Y := Nav_State.Y - Instruction.Step;
            when East =>
               New_Nav_State.X := Nav_State.X + Instruction.Step;
            when West =>
               New_Nav_State.X := Nav_State.X - Instruction.Step;
            end case; -- Nav_State.Heading
      end case; -- Instruction.Direction
      return New_Nav_State;
   end Move;

   function Move_2 (Nav_State : in Nav_States;
                  Instruction : in Instructions) return Nav_States is

      New_Nav_State : Nav_States := Nav_State;

   begin -- Move_2
      case Instruction.Direction is
         when North =>
            New_Nav_State.Way_Y := Nav_State.Way_Y + Instruction.Step;
         when South =>
            New_Nav_State.Way_Y := Nav_State.Way_Y - Instruction.Step;
         when East =>
            New_Nav_State.Way_X := Nav_State.Way_X + Instruction.Step;
         when West =>
            New_Nav_State.Way_X := Nav_State.Way_X - Instruction.Step;
         when Left | Right =>
            declare
               Rotation : Angles;
            begin -- rotation
               if Instruction.Direction = Left then
                  Rotation := - Instruction.Angle;
               else
                  Rotation := Instruction.Angle;
               end if; -- Instruction.Direction = Left
               case Rotation is
               when 0 =>
                  null; -- do nothing Way_X and Way_Y already assigned old
                  -- values also may not exist in input data.
               when 90 =>
                  New_Nav_State.Way_X := Nav_State.Way_Y;
                  New_Nav_State.Way_Y := -Nav_State.Way_X;
               when 180 =>
                  New_Nav_State.Way_X := -Nav_State.Way_X;
                  New_Nav_State.Way_Y := -Nav_State.Way_Y;
               when 270 =>
                  New_Nav_State.Way_X := -Nav_State.Way_Y;
                  New_Nav_State.Way_Y := Nav_State.Way_X;
               end case; -- Rotation
            end; -- rotation
         when Forward =>
            New_Nav_State.X := Nav_State.X + Instruction.Step * Nav_State.Way_X;
            New_Nav_State.Y := Nav_State.Y + Instruction.Step * Nav_State.Way_Y;
      end case; -- Instruction.Direction
      return New_Nav_State;
   end Move_2;

   Input_File : File_Type;
   Direction : Directions;
   Instruction_List : Instruction_Lists.Vector := Empty_Vector;
   Nav_State : Nav_States;

begin -- December_12
   Open (Input_File, In_File, "december_12.txt");
   while not End_Of_File (Input_File) loop
      Get (Input_File, Direction);
      declare
         Instruction : Instructions (Direction);
         -- has to be declared with a defined direction, that is, after
         -- direction is known.
      begin -- read steps or angles
         if Direction in Left | Right then
            Angle_IO.Get (Input_File, Instruction.Angle);
         else
            Step_IO.Get (Input_File, Instruction.Step);
         end if;
         Append (Instruction_List, Instruction);
      end; -- read steps or angles
      Skip_Line (Input_File);
   end loop; -- not End_Of_File (Input_File)
   Close (Input_File);
   Nav_State := (X => 0, Y => 0, Heading => East, Way_X => 10, Way_Y => 1);
   -- Way_X and Way_Y initialisation not required for part one
   for I in Iterate (Instruction_List) loop
      Nav_State := Move (Nav_State, Instruction_List (I));
   end loop; -- I in Iterate (Instruction_List)
   Put_Line ("Distance (Part one):" &
               Coordinates'Image (abs (Nav_State.X) + abs (Nav_State.Y)));
   Nav_State := (X => 0, Y => 0, Heading => East, Way_X => 10, Way_Y => 1);
   -- Heading initialisation not required for part two
   for I in Iterate (Instruction_List) loop
      Nav_State := Move_2 (Nav_State, Instruction_List (I));
   end loop; -- I in Iterate (Instruction_List)
   Put_Line ("Distance (Part two):" &
               Coordinates'Image (abs (Nav_State.X) + abs (Nav_State.Y)));
end December_12;
