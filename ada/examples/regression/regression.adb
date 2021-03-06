with Regression_Library; use Regression_Library;

package body Regression is

    function "&" (Left, Right : String) return String is
    begin
        return Left;
    end "&";

    package body Outer is

        function OuterFun (Input : Boolean) return Boolean is
        begin
            return False;
        end OuterFun;

        procedure OuterProc (Input : in Boolean) is
        begin
            return;
        end OuterProc;

        package body Nested is

            procedure NestedProc (Input : in Boolean) is
            begin
                OuterProc(Input);
            end NestedProc;

            function NestedFun (Input : in Boolean; InputFun : FunctionType) return Boolean is
            begin
                return (LibraryFun(Input) or OuterFun(Input)) or InputFun(0);
            end NestedFun;

        end Nested;

    end Outer;

end Regression;
