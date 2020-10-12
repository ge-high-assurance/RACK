with Regression_Library;

package Regression is

    package Outer is

        function OuterFun (Input : Boolean) return Boolean;

        procedure OuterProc (Input : in Boolean);

        package Nested is

            procedure NestedProc (Input : in Boolean);

            function NestedFun (Input : in Boolean) return Boolean;

        end Nested;

    end Outer;

end Regression;
