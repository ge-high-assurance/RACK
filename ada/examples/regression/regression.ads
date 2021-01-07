with Regression_Library;

package Regression is

    function "&" (Left, Right : String) return String;

    package Outer is

        function OuterFun (Input : Boolean) return Boolean;

        procedure OuterProc (Input : in Boolean);

        package Nested is

            procedure NestedProc (Input : in Boolean);

            type FunctionType is access function(Input : Integer) return Boolean;

            function NestedFun (Input : in Boolean; InputFun : FunctionType) return Boolean;

        end Nested;

    end Outer;

end Regression;
