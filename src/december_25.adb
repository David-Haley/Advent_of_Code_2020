with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

procedure December_25 is

   subtype Cryptos is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   subtype Loop_Sizes is Positive;
   Divisor : constant Cryptos := 20201227;
   package Crypto_IO is new Ada.Text_IO.Integer_IO (Cryptos);
   use Crypto_IO;

   Input_File : File_Type;

   function Transform (Subject : in Cryptos;
                       Loop_Size : in Loop_Sizes) return Cryptos is

      Value : Cryptos := 1;

   begin -- Transform
      for L in Loop_Sizes range 1 .. Loop_Size loop
         Value := Value * Subject;
         Value := Value mod Divisor;
      end loop; --  C in Loop_Sizes range 1 .. Loop_Size
      return Value;
   end Transform;

   function Find_Loop_Size (Public_Key : in Cryptos) return Loop_Sizes is

      Loop_Size : Loop_Sizes := 1;
      Subject : constant Cryptos := 7;
      Value : Cryptos := 1;

   begin -- Find_Loop_Size
      loop
         Value := Value * Subject;
         Value := Value mod Divisor;
         exit when Public_Key = Value;
         Loop_Size := Loop_Size + 1;
      end loop;
      return Loop_Size;
   end Find_Loop_Size;

   Card_Public_Key, Door_Public_Key,
   Card_Private_Key, Door_Private_Key : Cryptos;
   Card_Loop_Size : Loop_Sizes;
   Door_Loop_Size : Loop_Sizes;

begin -- December_25
   Open (Input_File, In_File, "december_25.txt");
   -- Open (Input_File, In_File, "example_25.txt");
   Get (Input_File, Card_Public_Key);
   Skip_Line (Input_File);
   Get (Input_File, Door_Public_Key);
   Close (Input_File);
   Card_Loop_Size := Find_Loop_Size (Card_Public_Key);
   Door_Loop_Size := Find_Loop_Size (Door_Public_Key);
   Put_Line ("card loop size:" & Card_Loop_Size'img);
   Put_Line ("door loop size:" & Door_Loop_Size'img);
   Card_Private_Key := Transform (Door_Public_Key, Card_Loop_Size);
   Door_Private_Key := Transform (Card_Public_Key, Door_Loop_Size);
   Assert (Card_Private_Key = Door_Private_Key,
           "Mismatch of private key calculations");
   Put_Line ("Private key:" & Card_Private_Key'img);

end December_25;
