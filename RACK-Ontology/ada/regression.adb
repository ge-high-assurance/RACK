package Outer is
    function OuterFun (Input : Boolean) return Boolean;
    procedure OuterProc (Input : in Boolean);
    package Nested is
        procedure NestedProc ();
        function NestedFun return Boolean;
    end Nested;
end Outer;

package body Outer is

    function OuterFun (Input : Boolean) return Boolean is
    begin
        return False;
    end OuterFun;

    procedure OuterProc (Input : in Boolean)
    begin
        return;
    end OuterProc;

    package body Nested is

        procedure NestedProc (Input : in Boolean) is
        begin
            OuterProc(Input);
        end NestedProc;

        function Nested_Fun (Input : in Boolean) return Boolean is
        begin
            return OuterFun(Input);
        end Nested_Fun;

    end Nested;

end Outer;

package Outer.Child is

end Outer.Child;
