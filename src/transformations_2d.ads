generic

type Pixel_Coordinates is range <>;

package Transformations_2D is

   type Images is array (Pixel_Coordinates, Pixel_Coordinates) of Boolean;

   subtype Orientations is Natural range 0 .. 7;

   function Transform (Image : in Images;
                       Orientation : Orientations) return Images;

end Transformations_2D;
