with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Calendar;           use Ada.Calendar;
with Ada.Containers.Vectors;

procedure Show_Type_Invariant is

   package Courses is
      type Course is private
        with Type_Invariant => Check (Course);

      type Course_Container is private;

      procedure Add (CC : in out Course_Container; C : Course);

      function Init
        (Name : String; Start_Date, End_Date : Time) return Course;

      function Check (C : Course) return Boolean;

   private
      type Course is record
         Name       : Unbounded_String;
         Start_Date : Time;
         End_Date   : Time;
      end record;

      function Check (C         : Course) return Boolean is
        (C.Start_Date <= C.End_Date);

      package Course_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Course);

      type Course_Container is record
         V : Course_Vectors.Vector;
      end record;
   end Courses;

   package body Courses is
      procedure Add (CC : in out Course_Container; C : Course) is
      begin
        CC.V.Append (C);
      end Add;

      function Init
        (Name : String; Start_Date, End_Date : Time) return Course is
      begin
        return Course'(Name       => To_Unbounded_String (Name),
                       Start_Date => Start_Date,
                       End_Date   => End_Date);
      end Init;
   end Courses;

   use Courses;

   CC : Course_Container;

begin

   Add (CC,
        Init (Name       => "Intro to Photography",
              Start_Date => Time_Of (2018, 5, 1),
              End_Date   => Time_Of (2018, 5, 10)));

   --  This should trigger an error in the type-invariant check
   Add (CC,
        Init (Name       => "Intro to Video Recording",
              Start_Date => Time_Of (2019, 5, 1),
              End_Date   => Time_Of (2018, 5, 10)));

end Show_Type_Invariant;
