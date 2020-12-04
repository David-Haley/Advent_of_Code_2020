with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Assertions; use Ada.Assertions;

procedure December_04 is

   type Passport_Fields is (byr, iyr, eyr, hgt, hcl, ecl, pid, cid);
   type Field_Arrays is array (Passport_Fields) of Boolean;

   type Eye_Colours is (amb, blu, brn, gry, grn, hzl, oth);

   package Field_IO is new Ada.Text_IO.Enumeration_IO (Passport_Fields);
   use Field_IO;

   package Eye_Colour_IO is new Ada.Text_IO.Enumeration_IO (Eye_Colours);

   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);

   function All_Present (Field_Array : in Field_Arrays) return Boolean is

      Result : Boolean := True;

   begin -- All_Present
      for I In Passport_Fields loop
         Result := Result and Field_Array (I);
      end loop; -- I In Passport_Fields
      return Result;
   end All_Present;

   procedure Split (Field_Pair : in Unbounded_String;
                    Field : out Passport_Fields;
                    Value : out Unbounded_String) is

      Last_Ch : Positive;

   begin -- Split
      Get (To_String (Field_Pair), Field, Last_Ch);
      Assert (Element (Field_Pair, Last_Ch + 1) = ':', "Expected ':'");
      Value := Unbounded_Slice (Field_Pair, Last_Ch + 2, Length (Field_Pair));
      Trim (Value, Both);
   end Split;

   Input_File : File_Type;
   Text, Value : Unbounded_String;
   Delimiter : constant Character_Set := To_Set (' ');
   Start_At, First : Positive;
   Last : Natural;
   Init_Field : constant Field_Arrays := (cid => true, others => False);
   Field_Array : Field_Arrays := Init_Field;
   -- cid is optional
   Field_Index : Passport_Fields;
   Valid_Count_1, Valid_Count_2 : Natural := 0;
   Valid_2 : Boolean := True;

begin -- December_04
   Open (Input_File, In_File, "december_04.txt");
   while not End_Of_File (Input_File) loop
      Start_At := 1;
      Get_Line (Input_File, Text);
      loop -- one Field
         Find_Token (Text, Delimiter, Start_At, Outside, First, Last);
         if Last > 0 then
            -- token found
            Split (Unbounded_Slice (Text, First, Last), Field_Index, Value);
            Field_Array (Field_index) := True;
            case Field_Index is
               when byr | iyr | eyr =>
                  Valid_2 := Valid_2 and Length (Value) = 4;
                  declare
                     Year, Last_Ch : Positive;
                  begin -- Year
                     Positive_IO.Get (To_String (Value), Year, Last_Ch);
                     Valid_2 := Valid_2 and
                       (
                          (Field_Index = byr and Year >= 1920 and Year <= 2002)
                        or
                          (Field_Index = iyr and Year >= 2010 and Year <= 2020)
                        or
                          (Field_Index = eyr and Year >= 2020 and Year <= 2030)
                       );
                  exception
                     when others =>
                        Valid_2 := False;
                  end; -- Year
               when hgt =>
                  declare
                     Height, Last_Ch : Positive;
                  begin -- Height
                     Positive_IO.Get (To_String (Value), Height, Last_Ch);
                     if Slice (Value, Last_Ch + 1, Length (Value)) = "cm" then
                        Valid_2 := Valid_2 and Height >= 150 and Height <= 193;
                     elsif Slice (Value, Last_Ch + 1, Length (Value)) = "in"
                     then
                        Valid_2 := Valid_2 and Height >= 59 and Height <= 76;
                     else
                        Assert (False, "unknown Height units " &
                                  Slice (Value, Last_Ch, Length (Value)));
                     end if; -- Slice (Value, Last_Ch, Length (Value)) = ...
                  exception
                     when others =>
                        Valid_2 := False;
                  end; -- height
               when hcl =>
                  Valid_2 := Valid_2 and Element (Value, 1) = '#' and
                    Ada.Strings.Unbounded.Count (Unbounded_Slice (Value, 2,
                                                 Length (Value)),
                                                 Hexadecimal_Digit_Set) = 6;
               when ecl =>
                  declare
                     Eye_Colour : Eye_Colours;
                     Last_Ch : Positive;
                  begin -- Eye Colour
                     Eye_Colour_IO.Get (To_String (Value), Eye_Colour, Last_Ch);
                  exception
                     when others =>
                        Valid_2 := False;
                  end; -- Eye Colour
               when pid =>
                  Valid_2 := Valid_2 and
                    Ada.Strings.Unbounded.Count (Value, Decimal_Digit_Set) = 9;
               when cid =>
                  null; -- No checking
            end case; -- Field_Index
            Start_At := Last + 1;
         end if; -- Last > 0
         exit when Last = 0 or Start_At >= Length (Text);
      end loop; -- one Field
      if Length (Text) = 0 or End_Of_File (Input_File) then
         if All_Present (Field_Array) then
            Valid_Count_1 := Valid_Count_1 + 1;
            if Valid_2 then
               Valid_Count_2 := Valid_Count_2 + 1;
            end if; -- Valid_2
         end if; -- All_Present (Field_Array)
         Field_Array := Init_Field;
         Valid_2 := True;
         -- Start of a new passport
      end if; -- Length (Text) = 0 or End_Of_File (Input_File)
   end loop; -- not End_Of_File (Input_File)
   Put_Line ("Part One Valid Passports:" & Natural'Image (Valid_Count_1));
   Put_Line ("Part Two Valid Passports:" & Natural'Image (Valid_Count_2));
   Close (Input_File);
end December_04;
