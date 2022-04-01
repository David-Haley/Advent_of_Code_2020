with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;   use Ada.Text_IO.Unbounded_IO;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Assertions;             use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_19 is

   subtype Symbols is Character range 'a' .. 'b';
   subtype Rule_Ids is Natural;

   type Sides is (Left_Rule, Right_Rule);

   type Sub_Rule_Elements is record
      Exists   : Boolean  := False;
      Sub_Rule : Rule_Ids := Rule_Ids'Last;
   end record; -- Sub_Rule_Element

   type Sub_Rules is array (Sides) of Sub_Rule_Elements;

   type Alternative_Rules is array (Boolean) of Sub_Rules;

   type Rules is record
      Is_Symbol        : Boolean := False;
      Symbol           : Symbols;
      Alternative_Rule : Alternative_Rules;
   end record; -- Rule_Element;

   package Rule_Lists is new Ada.Containers.Ordered_Maps (Rule_Ids, Rules);
   use Rule_Lists;

   package Products is new
     Ada.Containers.Ordered_Sets (Unbounded_String);
   use Products;

   package Product_Lists is new
     Ada.Containers.Ordered_Maps (Rule_Ids, Products.Set);
   use Product_Lists;

   procedure Get_Input (Rule_List : out Rule_Lists.Map;
                       Message_Set : out Products.Set) is

      -- december_19 [Input_File_Name]

      Symbol_Set : constant Character_Set := To_Set ("ab");
      Space_Set  : constant Character_Set := To_Set (" ");

      Input_File      : File_Type;
      Text            : Unbounded_String;
      Start_At, First : Positive;
      Last            : Natural;
      This_Rule       : Rule_Ids;
      Rule            : Rules;
      Empty_Rule      : constant Rules :=
        (False, 'a',
         (((False, Rule_Ids'Last), (False, Rule_Ids'Last)),
          ((False, Rule_Ids'Last), (False, Rule_Ids'Last))));

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_19.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      loop --  -- read until blank line
         Get_Line (Input_File, Text);
         exit when Length (Text) = 0;
         Rule     := Empty_Rule;
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         This_Rule := Rule_Ids'Value (Slice (Text, First, Last));
         Start_At  := Last + 2; -- skip ':'
         Find_Token (Text, Symbol_Set, Start_At, Inside, First, Last);
         if Last /= 0 then
            Rule.Is_Symbol := True;
            Rule.Symbol    := Element (Text, First);
         else
            for A in Boolean loop
               Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
               for S in Sides loop
                  if Last /= 0 and then Element (Text, First) /= '|' then
                  Rule.Alternative_Rule (A) (S).Exists   := True;
                  Rule.Alternative_Rule (A) (S).Sub_Rule :=
                    Rule_Ids'Value (Slice (Text, First, Last));
                  Start_At := Last + 1;
                  Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
                  end if; -- Last /= 0 and then Element (Text, First) /= '|'
               end loop; -- S in Sides
               if Last /= 0 and then Element (Text, First) = '|' then
                  Start_At := Last + 1;
               end if; -- Last /= 0 and then Element (Text, First) = '|'
            end loop; -- A in Boolean
         end if; -- Last /= 0
         Insert (Rule_List, This_Rule, Rule);
      end loop; -- read until blank line
      Message_Set := Products.Empty_Set;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Include (Message_Set, Text);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   procedure Build_Products (Rule_List : in Rule_Lists.Map;
                            Product_List : Out Product_Lists.Map) is

      package QI is new Ada.Containers.Synchronized_Queue_Interfaces (Rule_Ids);
      package Rule_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (QI);
      use Rule_Queues;

      function All_Products_Exist (Product_List : in Product_Lists.Map;
                                   Alternative_Rule : in Alternative_Rules)
                                   return Boolean is

         Result : Boolean := True;

      begin -- All_Products_Exist
         for A in Boolean loop
            for S in Sides loop
               if Alternative_Rule (A) (S).Exists then
                  Result := Result and
                    Contains (Product_List, Alternative_Rule (A) (S).Sub_Rule);
               end if; -- Alternative_Rule (A) (S).Exists
            end loop; -- S in Sides
         end loop; -- A in Boolean
         return Result;
      end All_Products_Exist;

      function Expand (Set_L, Set_R : in Products.Set) return Products.set is

         Result : Products.Set := Products.Empty_Set;

      begin -- Expand
         for Sl in Iterate (Set_L) loop
            for Sr in Iterate (Set_R) loop
               Include (Result, Set_L (Sl) & Set_R (Sr));
            end loop; -- Sr in Iterate (Set_R)
         end loop; -- Sl in Iterate (Set_L)
         return Result;
      end Expand;

      Rule_Queue : Rule_Queues.Queue;
      Current_Rule : Rule_Ids;
      Message : Unbounded_String;
      Product_Array : array (Boolean) of Products.Set;

   begin -- Build_Products
      Product_List := Product_Lists.Empty_Map;
      for R in Iterate (Rule_List) loop
         if Rule_List (R).Is_Symbol then
            insert (Product_List, Key (R), Products.Empty_Set);
            Message := Null_Unbounded_String;
            Message := Message & Rule_List (R).Symbol;
            include (Product_List (Key (R)), Message);
            -- There may be a better way to convert a single character to an
            -- Unbounded_String, this works although it looks bad!
         else
            Rule_Queue.Enqueue (Key (R));
         end if; -- Rule_List (R).Is_Synbol
      end loop; -- R in Iterate (Rule_List)
      while Rule_Queue.Current_Use > 0 loop
         Rule_Queue.Dequeue (Current_Rule);
         if All_Products_Exist (Product_List,
                                Rule_List (Current_Rule).Alternative_Rule) then
            for A in Boolean loop
               Product_Array (A) := Products.Empty_Set;
               if Rule_List (Current_Rule).Alternative_Rule (A) (Left_Rule).
                 Exists
               then
                  if
                    Rule_List (Current_Rule).Alternative_Rule (A) (Right_Rule).
                    Exists
                  then
                     Product_Array (A):=
                       Expand (Product_List (Rule_List (Current_Rule).
                                 Alternative_Rule (A)
                               (Left_Rule).Sub_Rule),
                               Product_List (Rule_List (Current_Rule).
                                   Alternative_Rule (A)
                                 (Right_Rule).Sub_Rule));
                  else
                     Product_Array (A):=
                       Copy (Product_List (Rule_List (Current_Rule).
                               Alternative_Rule (A)
                             (Left_Rule).Sub_Rule));
                  end if; -- Rule_List (Current_Rule).Alternative_Rule (A) ...
               end if; -- Rule_List (Current_Rule).Alternative_Rule (A) ...
            end loop; -- A in Boolean
            include (Product_List, Current_Rule, Products.Empty_Set);
              Product_List (Current_Rule) :=
              Union (Product_Array (False), Product_Array (True));
         else
            Rule_Queue.Enqueue (Current_Rule);
            -- If it cannot be expanded now put it back and try again later.
         end if; -- All_Products_Exist (Product_List,
      end loop; -- Rule_Queue.Current_Use > 0
   end Build_Products;

   Rule_List : Rule_Lists.Map;
   Product_List : Product_Lists.Map;
   Message_Set, Valid_Set: Products.Set;

begin -- December_19
   Get_Input (Rule_List, Message_Set);
   Build_Products (Rule_List, Product_List);
   Valid_Set := Intersection (Product_List (0), Message_Set);
   Put_Line ("Part One (Valid messages)" & Length (Valid_Set)'Img);
   Put_CPU_Time;
end December_19;
