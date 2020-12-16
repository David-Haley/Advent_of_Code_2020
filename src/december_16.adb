with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Vectors;

procedure December_16 is

   subtype Values is Natural;

   type Valid_Ranges is record
      Field_Name : Unbounded_String;
      Low_1, High_1, Low_2, High_2 : Values;
   end record; -- Valid_Ranges

   package Validation_Lists is new
     Ada.Containers.Vectors (Positive, Valid_Ranges);
   use Validation_Lists;

   subtype Field_Indices is Positive range 1 .. 20;
   -- this is a cheat ideally the prpgram should discver this;
   type Tickets is array (Field_Indices) of Values;

   package Near_Ticket_Lists is new Ada.Containers.Vectors (Positive, Tickets);
   use Near_Ticket_Lists;

   procedure Read_Valid_Range (Input_File : in File_Type;
                               Validation_List : out Validation_Lists.Vector) is

      Text: Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Colon : Character_Set := To_Set (':');
      Valid_Range : Valid_Ranges;

   begin -- Read_Valid_Range
      Validation_List := Validation_Lists.Empty_Vector;
      Get_Line (Input_File, Text);
      while Length (Text) /= 0 loop
         Start_At := 1;
         Find_Token (Text, Colon, Start_At, Outside, First, Last);
         Valid_Range.Field_Name := Unbounded_Slice (Text, First, Last);
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
         Append (Validation_List, Valid_Range);
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
                        Near_Ticket_List : out Near_Ticket_Lists.Vector) is

      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Ticket : Tickets;

   begin -- Read_Near_Tickets
      Near_Ticket_List := Near_Ticket_Lists.Empty_Vector;
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

   function Ticket_Error_Rate (Ticket : in Tickets;
                               Validation_List : in Validation_Lists.Vector)
                               return Natural is

      -- Returns the som of erroneous fields, that is ang ticket field that does
      -- not satisfies at least one validation range.

      Valid : Boolean;
      Error_Count : Natural := 0;

   begin -- Ticket_Error_Rate
      for I in Field_Indices loop
         Valid := False;
         for V in Iterate (Validation_List) loop
            Valid := Valid or
              (Ticket (I) >= Validation_List (V).Low_1 and
                   Ticket (I) <= Validation_List (V).High_1) or
              (Ticket (I) >= Validation_List (V).Low_2 and
                   Ticket (I) <= Validation_List (V).High_2);
         end loop; -- I in Field_Indices
         if not Valid then
            Error_Count := Error_Count + Ticket (I);
         end if; -- not Valid
      end loop; -- I in Field_Indices
      return Error_Count;
   end  Ticket_Error_Rate;

   Input_File : File_Type;
   Validation_List : Validation_Lists.Vector;
   Your_Ticket : Tickets;
   Near_Ticket_List : Near_Ticket_Lists.Vector;
   Error_Rate : Natural;

begin -- December_16
   Open (Input_File, In_File, "december_16.txt");
   -- Open (Input_File, In_File, "example_16.txt");
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
end December_16;
