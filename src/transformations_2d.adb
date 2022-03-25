
package body Transformations_2D is

   Bias : constant Integer :=
     Integer (Pixel_Coordinates'First + Pixel_Coordinates'Last);

   function Transform (Image : in Images;
                       Orientation : Orientations) return Images is

      type Axies is (X, Y);

      subtype Rotations is Natural range 0 .. 3;

      subtype Flips is Boolean;

      subtype Transform_Elements is Integer range -1 .. 1;

      type Transform_Matrix is Array (Axies, Axies) of Transform_Elements;

      function Basic_Transform (Image : in Images;
                                Matrix : in Transform_Matrix) return Images is

         type Coordinates is array (Axies) of Pixel_Coordinates;

         C_In, C_Out : Coordinates;

         Result : Images;

      begin -- Basic_Transform
         for Xi in Pixel_Coordinates loop
            for Yi in Pixel_Coordinates loop
               C_In := (Xi, Yi);
               for Axis_In in Axies loop
                  for Axis_Out in Axies loop
                     if Matrix (Axis_In, Axis_Out) = 1 then
                        C_Out (Axis_Out) := C_In (Axis_In);
                     elsif Matrix (Axis_In, Axis_Out) = -1 then
                        C_Out (Axis_Out) :=
                          Pixel_Coordinates (Bias - Integer (C_In (Axis_In)));
                     end if; -- Matrix (Axis_In, Axis_Out) = 1
                  end loop; --  Axis_Out in Axies
               end loop; -- Axis_In in Axies
               Result (C_Out (X), C_Out (Y)) := Image (C_In (X), C_In (Y));
            end loop; -- Yi in Pixel_Coordinates
         end loop; -- Xi in Pixel_Coordinates
         return Result;
      end Basic_Transform;

      Rotation_Table : constant array (Rotations) of Transform_Matrix :=
        (
         00 => (( 1,  0),
                ( 0,  1)), -- no rotation
         01 => (( 0, -1),
                ( 1,  0)),
         02 => ((-1,  0),
                ( 0, -1)),
         03 => (( 0,  1),
                (-1,  0)));

      X_Flip : constant Transform_Matrix :=
        (( 1,  0),
         ( 0, -1));

      Result : Images;

   begin -- Transform
      Result := Basic_Transform (Image, Rotation_Table (Orientation mod
                                   (Rotations'Last + 1)));
      if Orientation / (Rotations'Last + 1) > 0 then
         Result := Basic_Transform (Result, X_Flip);
      end if; -- Orientation / (Rotations'Last + 1) > 0
      return Result;
   end Transform;

end Transformations_2D;
