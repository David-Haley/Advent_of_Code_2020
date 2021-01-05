with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

procedure December_16 is

   subtype Values is Natural;
   subtype Field_Names is String;

   type Valid_Ranges is record
      Low_1, High_1, Low_2, High_2 : Values;
   end record; -- Valid_Ranges

   package Validation_Lists is new
     Ada.Containers.Indefinite_Ordered_Maps (Field_Names, Valid_Ranges);
   use Validation_Lists;

   subtype Field_Indices is Positive range 1 .. 20;
   -- this is a cheat ideally the program should discover this;
   type Tickets is array (Field_Indices) of Values;

   package Range_Sets is new Ada.Containers.Ordered_Sets (Field_Indices);
   use Range_Sets;

   package Field_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Field_Names, Range_Sets.Set);
   use Field_Maps;

   package Ticket_Lists is new
     Ada.Containers.Vectors (Positive, Tickets);
   use Ticket_Lists;

   procedure Read_Valid_Range (Input_File : in File_Type;
                               Validation_List : out Validation_Lists.Map) is

      Text: Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Colon : Character_Set := To_Set (':');
      Valid_Range : Valid_Ranges;

   begin -- Read_Valid_Range
      Validation_List := Validation_Lists.Empty_Map;
      Get_Line (Input_File, Text);
      while Length (Text) /= 0 loop
         Start_At := 1;
         Find_Token (Text, Colon, Start_At, Outside, First, Last);
         declare -- read one Field_Name
            Field_Name : Field_Names := Slice (Text, First, Last);
         begin -- read one Field_Name
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Valid_Range.Low_1 := Positive'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Valid_Range.High_1 := Positive'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Valid_Range.Low_2 := Positive'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Valid_Range.High_2 := Positive'Value (Slice (Text, First, Last));
            Include (Validation_List, Field_Name, Valid_Range);
         end; -- read one Field_Name
         Get_Line (Input_File, Text);
      end loop; -- Length (Text) /= 0
   end Read_Valid_Range;

   procedure Read_Your_Ticket (Input_File : in File_Type;
                        Ticket : out Tickets) is

      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_Your_Ticket
      Get_Line (Input_File, Text);
      while Index (Text, "your ticket:") = 0 loop
         Get_Line (Input_File, Text);
      end loop; -- Index (Text, "your ticket:") = 0
      Get_Line (Input_File, Text);
      Start_At := 1;
      for I in Field_Indices loop
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Ticket (I) := Values'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
      end loop; -- I in Field_Indices
   end Read_Your_Ticket;

   procedure Read_Near_Tickets (Input_File : in File_Type;
                        Near_Ticket_List : out Ticket_Lists.Vector) is

      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Ticket : Tickets;

   begin -- Read_Near_Tickets
      Near_Ticket_List := Ticket_Lists.Empty_Vector;
      Get_Line (Input_File, Text);
      while Index (Text, "nearby tickets:") = 0 loop
         Get_Line (Input_File, Text);
      end loop; -- Index (Text, "your ticket:") = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         for I in Field_Indices loop
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Ticket (I) := Values'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
         end loop; -- I in Field_Indices
         Append (Near_Ticket_List, Ticket);
      end loop; -- not End_Of_File (Input_File)
   end Read_Near_Tickets;

   function Valid_Field (Value : in Values;
                         Validation_List : in Validation_Lists.Map;
                         Field : in Validation_Lists.Cursor) return Boolean is

   begin -- Valid_Field
      return (Value in Validation_List (Field).Low_1 ..
                Validation_List (Field).High_1) or
        (Value in Validation_List (Field).Low_2 ..
             Validation_List (Field).High_2);
   end Valid_Field;

   function Ticket_Error_Rate (Ticket : in Tickets;
                               Validation_List : in Validation_Lists.Map)
                               return Natural is

      -- Returns the sum of erroneous fields, that is any ticket field that does
      -- not satisfies at least one validation range. Note VERY CAREFULLY 0 does
      -- not mean that a ticket is valid a 0 value is never valid and does not
      -- change sum.

      Valid : Boolean;
      Error_Count : Natural := 0;

   begin -- Ticket_Error_Rate
      for I in Field_Indices loop
         Valid := False;
         for V in Iterate (Validation_List) loop
            Valid := Valid or Valid_Field (Ticket (I), Validation_List, V);
         end loop; -- I in Field_Indices
         if not Valid then
            Error_Count := Error_Count + Ticket (I);
         end if; -- not Valid
      end loop; -- I in Field_Indices
      return Error_Count;
   end  Ticket_Error_Rate;

   procedure Build_Master_Map (Near_Ticket_List : in Ticket_Lists.Vector;
                               Validation_List : in Validation_Lists.Map;
                               Master_Field_Map : out Field_Maps.Map) is

      Valid_Columns : Range_Sets.Set := Range_Sets.Empty_Set;
      Valid_Map : Field_Maps.Map := Field_Maps.Empty_Map;

   begin -- Build_Master_Map
      Master_Field_Map := Field_Maps.Empty_Map;
      for V in Iterate (Validation_List) loop
         Include (Master_Field_Map, Key (V), Range_Sets.Empty_Set);
         for I in Field_Indices loop
            -- Set all elements in set
            Include (Master_Field_Map (Key (V)), I);
            Include (Valid_Map, Key (V), Range_Sets.Empty_Set);
         end loop; -- V in Iterate (Validation_List)x
      end loop; -- V in Iterate (Validation_List)
      for T in Iterate (Near_Ticket_List) loop
         Clear (Valid_Columns);
         for V in Iterate (Validation_List) loop
            Clear (Valid_Map (Key (V)));
         end loop; -- V in Iterate (Validation_List)
         for I in Field_Indices loop
            for V in Iterate (Validation_List) loop
               if Valid_Field (Near_Ticket_List (T) (I), Validation_List, V)
               then
                  Include (Valid_Columns, I); -- to test ticket validity
                  Include (Valid_Map (Key (V)), I);
               end if; -- Valid_Field (Near_Ticket_List (T) (I), ...
            end loop; -- V in Iterate (Validation_List)
         end loop; -- I in Field_Indices
         if Length (Valid_Columns) = Count_Type (Field_Indices'Last) then
            -- Only include tickets which are valid, that is, every column
            -- passes at least one range check.
            for V in Iterate (Validation_List) loop
               Intersection (Master_Field_Map (Key (V)), Valid_Map (Key (V)));
            end loop; -- V in Iterate (Validation_List)
         end if; -- Length (Valid_Columns) = Count_Type (Field_Indices'Last)
      end loop; -- T in Iterate (Near_Ticket_List)
   end Build_Master_Map;

   procedure Unique_Columns (Master_Field_Map : in out Field_Maps.Map) is

      function All_One (Field_Map : in Field_Maps.Map) return Boolean is

         -- Returns True if all field names are matched to a unique column
         -- number, that is, the sets of colums contain only one element.

      Result : Boolean := True;

      begin -- All_One
         for F in Iterate (Field_Map) loop
            Result := Result and Length (Field_Map (F)) = 1;
         end loop; -- F in Iterate (Field_Map)
         return Result;
      end All_One;

   begin -- Unique_Columns
      loop -- resolve destination fields
         for F1 in Iterate (Master_Field_Map) loop
            for F2 in Iterate (Master_Field_Map) loop
               if F1 /= F2 and then Length (Master_Field_Map (F1)) = 1 then
                  Exclude (Master_Field_Map (F2),
                           First_Element (Master_Field_Map (F1)));
               end if; -- F1 /= F2 and then Length (Master_Field_Map (F1)) = 1
            end loop; --  F2 in Iterate (Master_Field_Map)
         end loop; -- F1 in Iterate (Master_Field_Map)
         exit when All_One (Master_Field_Map);
      end loop; -- resolve destination fields
   end Unique_Columns;

   Input_File : File_Type;
   Validation_List : Validation_Lists.Map;
   Your_Ticket : Tickets;
   Near_Ticket_List : Ticket_Lists.Vector;
   Error_Rate : Natural;
   Master_Field_Map : Field_Maps.Map := Field_Maps.Empty_Map;
   Product : Long_Long_Integer := 1;

begin -- December_16
   Open (Input_File, In_File, "december_16.txt");
   Read_Valid_Range (Input_File, Validation_List);
   Read_Your_Ticket (Input_File, Your_Ticket);
   Read_Near_Tickets (Input_File, Near_Ticket_List);
   Close (Input_File);
   Error_Rate := 0;
   for N in Iterate (Near_Ticket_List) loop
      Error_Rate := Error_Rate +
        Ticket_Error_Rate (Near_Ticket_List (N), Validation_List);
   end loop; -- N in Iterate (Near_Ticket_List)
   Put_Line ("Error rate (Part one):" & Natural'Image (Error_Rate));
   -- Part Two
   Build_Master_Map (Near_Ticket_List, Validation_List, Master_Field_Map);
   Unique_Columns (Master_Field_Map);
   for F in Iterate (Master_Field_Map) loop
      if Index (Key (F), "departure") /= 0 then
         Product := Product * Long_Long_Integer
           (Your_Ticket (First_Element (Master_Field_Map (F))));
         -- Contains exactly one element
      end if; -- Index (Key (F), "departure") /= 0
   end loop; -- F in Iterate (Master_Field_Map)
   Put_Line ("Product of destination fields:" & Product'Img);
end December_16;
