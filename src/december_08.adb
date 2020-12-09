with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure December_08 is

   type Op_Codes is (I_Acc, I_Jmp, I_Nop);
   subtype Addresses is Positive;
   subtype Operands is Integer;
   type Instructions is record
      Op_Code : Op_Codes;
      Operand : Operands;
   end record; -- Instructions;

   Mnemonic : constant array (Op_Codes) of String (1 .. 3)
     := (I_Acc => "acc", I_Jmp => "jmp", I_Nop => "nop");

   package Program_Stores is new
     Ada.Containers.Vectors (Addresses, Instructions);
   use Program_Stores;

   package Address_Sets is new Ada.Containers.Ordered_Sets (Positive);
   use Address_Sets;

   package Operand_IO is new Ada.Text_IO.Integer_IO (Operands);

   procedure Load (File_Name : in String;
                   Program_Store : out Program_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Instruction : Instructions;
      First, Last : Natural;

   begin -- Load
      Open (Input_File, In_File, File_Name);
      Program_Store := Empty_Vector;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Instruction.Op_Code := Op_Codes'First;
         loop -- get Op_Code
            First := Index (Text, Mnemonic (Instruction.Op_Code));
            exit when First > 0;
            assert (Instruction.Op_Code < Op_Codes'Last, "Bad Op_Code at line:"
                    & Positive'Image (Last_Index (Program_Store) + 1));
            Instruction.Op_Code := Op_Codes'Succ (Instruction.Op_Code);
         end loop; -- get Op_Code
         begin -- get Operand
            Operand_IO.Get (Slice (Text,
                            First + MNemonic (Instruction.Op_Code)'Length,
                            Length (Text)),
                            Instruction.Operand, Last);
         exception
            when others =>
               Put_Line ("Bad Operand at line:" &
                           Positive'Image (Last_Index (Program_Store) + 1));
         end; -- get Operandd
         Append (Program_Store, Instruction);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Load;

   procedure Execute (Program_Store : in Program_Stores.Vector;
                      Address_Set : out Address_Sets.Set) is

      package Address_Sets is new Ada.Containers.Ordered_Sets (Positive);
      use Address_Sets;

      Accumulator,Saved_Accumulator : Integer := 0;
      Instruction_Pointer : Addresses := 1;
      Trace_File : File_Type;

   begin -- Execute
      Address_Set := To_Set (Instruction_Pointer);
      Create (Trace_File, Out_File, "trace_08.txt");
      loop -- One_Instruction
         Saved_Accumulator := Accumulator;
         Put_Line (Trace_File, Addresses'Image (Instruction_Pointer) & ": " &
                     Mnemonic (Program_Store (Instruction_Pointer).Op_Code) &
                     " " &                     Operands'Image (Program_Store
                     (Instruction_Pointer).Operand) &
                     " Acc: " & Operands'Image (Accumulator));
         case Program_Store (Instruction_Pointer).Op_Code is
            when I_Acc =>
               Accumulator := Accumulator +
                 Program_Store (Instruction_Pointer).Operand;
               Instruction_Pointer := Instruction_Pointer + 1;
            when I_Jmp =>
               Instruction_Pointer := Instruction_Pointer +
                 Program_Store (Instruction_Pointer).Operand;
            when I_Nop =>
               Instruction_Pointer := Instruction_Pointer + 1;
         end case; -- Program_Store (Instruction_Pointer).Op_Code
         exit when Contains (Address_Set, Instruction_Pointer);
         Include (Address_Set, Instruction_Pointer);
      end loop; -- One_Instruction
      Put_Line ("Accumulator:" & Operands'Image (Saved_Accumulator));
      Put_Line ("Instruction_Pointer:" & Addresses'Image (Instruction_Pointer));
      Close (Trace_File);
   end Execute;

   procedure Execute_2 (Program_Store : in Program_Stores.Vector) is

      package Address_Sets is new Ada.Containers.Ordered_Sets (Positive);
      use Address_Sets;

      Accumulator : Integer := 0;
      Instruction_Pointer : Addresses := 1;
      Address_Set : Address_Sets.Set := To_Set (Instruction_Pointer);

   begin -- Execute_2
      loop -- One_Instruction
         case Program_Store (Instruction_Pointer).Op_Code is
            when I_Acc =>
               Accumulator := Accumulator +
                 Program_Store (Instruction_Pointer).Operand;
               Instruction_Pointer := Instruction_Pointer + 1;
            when I_Jmp =>
               Instruction_Pointer := Instruction_Pointer +
                 Program_Store (Instruction_Pointer).Operand;
            when I_Nop =>
               Instruction_Pointer := Instruction_Pointer + 1;
         end case; -- Program_Store (Instruction_Pointer).Op_Code
         exit when Contains (Address_Set, Instruction_Pointer) or
           Instruction_Pointer = Last_Index (Program_Store) + 1;
         Include (Address_Set, Instruction_Pointer);
      end loop; -- One_Instruction
      if Instruction_Pointer = Last_Index (Program_Store) + 1 then
         Put_Line ("Accumulator:" & Operands'Image (Accumulator));
      end if; -- Instruction_Pointer = Last_Index (Program_Store) + 1
   end Execute_2;

   Program_Store, Program_Store_2: Program_Stores.Vector := Empty_Vector;
   Address_Set : Address_Sets.Set := Empty_Set;

begin -- December_08
   Load ("december_08.txt", Program_Store);
   Execute (Program_Store, Address_Set);
   for I in Iterate (Address_Set) loop
      if Program_Store (Element (I)).Op_Code = I_Jmp then
         Assign (Program_Store_2, Program_Store);
         Program_Store_2 (Element (I)). Op_Code := I_Nop;
      elsif Program_Store (Element (I)).Op_Code = I_Nop then
         Assign (Program_Store_2, Program_Store);
         Program_Store_2 (Element (I)). Op_Code := I_Jmp;
      end if; -- Program_Store (I).Op_Code = I_Jmp
      Execute_2 (Program_Store_2);
   end loop; --  I in Iterate (Address_Set)
end December_08;
