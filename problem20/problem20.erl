% Problem link: https://projecteuler.net/problem=20


% Problem Statement:
% n! means n × (n − 1) × ... × 3 × 2 × 1

% For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
% and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

% Find the sum of the digits in the number 100!

-module(problem20).
-export([main/1]).

main( N )
->
    Factorial = factorial( N ),
    sum_digits( Factorial )
.

factorial( N )      ->  factorial( N, 1 ).
factorial( 1, Acc ) ->  Acc;
factorial( N, Acc ) ->  factorial( N-1, Acc*N ).

sum_digits( Factorial ) -> sum_digits( integer_to_list(Factorial), 0).
sum_digits( [Current_Digit | Rest], Acc )
->
    sum_digits( Rest, Acc+list_to_integer([Current_Digit]) )
;
sum_digits( [], Acc ) -> Acc.
