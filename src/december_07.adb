with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;

procedure December_07 is

   subtype Quantities is Positive;

   package Quantitity_IO is new Ada.Text_IO.Integer_IO (Quantities);

   subtype Bags is Unbounded_String;
   type Content_Elements is record
      Quantity : Quantities;
      Bag : Bags;
   end record; -- Content_Elements

   package Content_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Content_Elements);
   use Content_Lists;

   package Enclosing_Lists is new
     Ada.Containers.Ordered_Maps (Bags, Content_Lists.List);
   use Enclosing_Lists;

   package Bag_Sets is new Ada.Containers.Ordered_Sets (Bags);
   use Bag_Sets;

   package Enclosed_By_Maps is new
     Ada.Containers.Ordered_Maps (Bags, Bag_Sets.Set);
   use Enclosed_By_Maps;

   procedure Clean (Bag : in out Bags) is

      Found : Natural;

   begin -- Clean
      Found := Index (Bag, "bag", Forward);
      if Found > 0 then
         Delete (Bag, Found, Length (Bag));
      else
         Found := Index (Bag, "bags", Forward);
         if Found > 0 then
            Delete (Bag, Found, Length (Bag));
         end if; -- Found > 0
      end if; -- Found > 0
      Trim (Bag, Both);
   end Clean;

   procedure Count_Bags (Bag : in Bags;
                         Enclosed_By_Map : in Enclosed_By_Maps.Map;
                         Bag_Set : in out Bag_Sets.Set) is

   begin -- Count_Bags
      if Contains (Enclosed_By_Map, Bag) then
         for B in Iterate (Enclosed_By_Map (Bag)) loop
            Include (Bag_Set, Element (B));
            Count_Bags (Element (B), Enclosed_By_Map, Bag_Set);
         end loop; -- B in Iterate (Enclosed_By_Map (Bag))
      end if; -- Contains (Enclosed_By_Map, Bag)
   end Count_Bags;

   procedure Count_Bags (Bag : in Bags;
                         Enclosing_List : in Enclosing_Lists.Map;
                         Count : out Natural) is

      Lower_Count : Natural;

   begin -- Count_Bags
      Count := 0;
      if Length (Enclosing_List (Bag)) > 0 then
         for L in Iterate (Enclosing_List (Bag)) loop
            Count_Bags (Enclosing_List (Bag) (L).Bag, Enclosing_List,
                        Lower_Count);
            Count := Count + Enclosing_List (Bag) (L).Quantity *
              (1 + Lower_Count);
         end loop; -- L in Iterate (Enclosing_List (Bag))
      end if; -- Length (Enclosing_List (Bag)) > 0
   end Count_Bags;

   Input_File : File_Type;
   Text : Unbounded_String;
   Contain : constant String := " contain ";
   No_Bags : constant String := "no other bags";
   Start_At, First : Positive;
   Last : Natural;
   Delimeter : Character_Set := To_Set (",.");
   Current_Bag : Bags;
   Enclosing_List : Enclosing_Lists.Map := Enclosing_Lists.Empty_Map;
   Content_Element : Content_Elements;
   Enclosed_By_Map : Enclosed_By_Maps.Map := Enclosed_By_Maps.Empty_Map;
   Shiny_Gold_Set : Bag_Sets.Set := Bag_Sets.Empty_Set;
   Part_Two_Bag_Count : Natural;

begin -- December_07
   Open (Input_File, In_File, "december_07.txt");
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Text);
      Start_At := 1;
      Last := Index (Text, Contain, Forward);
      Current_Bag := Unbounded_Slice (Text, Start_At, Last - 1);
      Clean (Current_Bag);
      Include (Enclosing_List, Current_Bag, Content_Lists.Empty_List);
      Start_At := Last + Contain'Length;
      while Start_At < Length (Text) and Index (Text, No_Bags, Forward) = 0 loop
         Find_Token (Text, Delimeter, Start_At, Outside, First, Last);
         Quantitity_IO.Get (Slice (Text, First, Last),
                            Content_Element.Quantity, First);
         First := First + 1;
         Start_At := Last + 1;
         Content_Element.Bag := Unbounded_Slice (Text, First, Last);
         Clean (Content_Element.Bag);
         Append (Enclosing_List (Find (Enclosing_List, Current_Bag)),
                 Content_Element);
      end loop; --  Start_At < Length (Text) and Index (Text, No_Bags, ...
   end loop; -- not End_Of_File (Input_File)
   -- Build a map of sets of bags enclosing the key bag
   for E in Iterate (Enclosing_List) loop
      for C in Iterate (Enclosing_List (E)) loop
         if Contains (Enclosed_By_Map, Enclosing_List (E) (C).Bag) then
            Include (Enclosed_By_Map (
                     Find (Enclosed_By_Map, Enclosing_List (E) (C).Bag)),
                     Key (E));
         else
            Include (Enclosed_By_Map, Enclosing_List (E) (C).Bag,
                     To_Set (Key (E)));
         end if; -- Contains (Enclosed_By_Map, Enclosing_List (E) (C).Bag)
      end loop; -- C in Iterate (Enclosing_List (E))
   end loop; -- E in Iterate (Enclosing_List)
   Count_Bags (To_Unbounded_String ("shiny gold"), Enclosed_By_Map,
               Shiny_Gold_Set);
   Put_Line ("Part One Bag_Count:" &
               Count_Type'Image (Length (Shiny_Gold_Set)));
   Count_Bags (To_Unbounded_String ("shiny gold"),  Enclosing_List,
               Part_Two_Bag_Count);
   Put_Line ("Part Two Bag_Count:" & Natural'Image (Part_Two_Bag_Count));
   Close (Input_File);
end December_07;
