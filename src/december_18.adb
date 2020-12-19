with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Vectors;

procedure December_18 is

   subtype Operands is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   function Evaluate (Expression : in Unbounded_String;
                      Start_At : in out Positive;
                      Part_Two : in Boolean := False) return Operands is

      Plus : constant Character := '+';
      Mult : constant Character := '*';
      None : constant Character := ' ';
      subtype Operators is Character with
        Static_Predicate => Operators in Plus | Mult | None;

      Lp : constant Character := '(';
      Rp : constant Character := ')';

      subtype Operand_Counts is Natural range 0 .. 2;
      subtype Operand_Indices is Operand_Counts range 1 .. 2;
      type Operand_Array is array (Operand_Indices) of Operands;

      procedure Operate (Current_Opertor : in out Operators;
                         Operand_Count : in out Operand_Counts;
                         Operand : in out Operand_Array) is

      begin -- Operate
         if Operand_Count = 2 then
            if Current_Opertor = Plus then
               Operand (1) := Operand (1) + Operand (2);
            elsif Current_Opertor = Mult then
               Operand (1) := Operand (1) * Operand (2);
            else
               Assert (False, "Two operands no operator");
            end if; -- Current_Opertor = Plus
            Current_Opertor := None;
            Operand_Count := 1;
         end if; -- Operand_Count = 2
      end Operate;

      Current_Opertor : Operators := None;
      Operand_Count : Operand_Counts := 0;
      Operand : Operand_Array := (0, 0); -- Initialisation for bebuging only
      First : Positive;
      Last : Natural;

   begin -- Evaluate
      while Start_At <= Length (Expression) loop
         if Is_In (Element (Expression, Start_At), Decimal_Digit_Set) then
            Operand_Count := Operand_Count + 1;
            Find_Token (Expression, Decimal_Digit_Set, Start_At, Inside,
                        First, Last);
            Start_At := Last + 1;
            Operand (Operand_Count) :=
              Operands'Value (Slice (Expression, First, Last));
            Operate (Current_Opertor, Operand_Count, Operand);
         elsif  Element (Expression, Start_At) = Plus then
            Assert (Operand_Count = 1, "+ with no first operand");
            Start_At := Start_At + 1;
            Current_Opertor := Plus;
         elsif  Element (Expression, Start_At) = Mult then
            Assert (Operand_Count = 1, "* with no first operand");
            Start_At := Start_At + 1;
            Current_Opertor := Mult;
            if Part_Two then
               -- Mult lower precedence operator, defer evaluation
               Operand_Count := Operand_Count + 1;
               Operand (Operand_Count) := Evaluate (Expression, Start_At,
                                                    Part_Two);
               Operate (Current_Opertor, Operand_Count, Operand);
               return Operand (1);
            end if;
         elsif Element (Expression, Start_At) = Lp then
            Start_At := Start_At + 1;
            Operand_Count := Operand_Count + 1;
            Operand (Operand_Count) := Evaluate (Expression, Start_At,
                                                 Part_Two);
            Operate (Current_Opertor, Operand_Count, Operand);
         elsif Element (Expression, Start_At) = Rp then
            Start_At := Start_At + 1;
            Operate (Current_Opertor, Operand_Count, Operand);
            return Operand (1);
         elsif  Element (Expression, Start_At) = ' ' then
            Start_At := Start_At + 1; -- skip white space
         else
            Assert (False, "Unknown token '" & Element (Expression, Start_At) &
                      "'");
         end if; -- Is_In (Element (Expression, Start_At) in Decimal_Digit_Set)
      end loop; -- Start_At < Length (Expression)
      Operate (Current_Opertor, Operand_Count, Operand);
      return Operand (1);
   end Evaluate;

   Input_File : File_Type;
   Expression : Unbounded_String;
   Start_At : Positive;
   Sum : Operands;

begin -- December_18
   Open (Input_File, In_File, "december_18.txt");
   Sum := 0;
   while not End_Of_File (Input_File) loop
       Get_Line (Input_File, Expression);
       Start_At := 1;
       Sum := Sum + Evaluate (Expression, Start_At);
   end loop; -- not End_Of_File (Input_File)
   Put_Line ("Sum (Part one):" & Operands'Image (Sum));
   Reset (Input_File);
   Sum := 0;
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Expression);
      Start_At := 1;
      Sum := Sum + Evaluate (Expression, Start_At, True);
   end loop; -- not End_Of_File (Input_File)
   Put_Line ("Sum (Part two):" & Operands'Image (Sum));
   Close (Input_File);
end December_18;
