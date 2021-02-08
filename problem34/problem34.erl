% Problem link: https://projecteuler.net/problem=34


% Problem Statement:
% 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

% Find the sum of all numbers which are equal to the sum of the factorial of 
% their digits.

% Note: As 1! = 1 and 2! = 2 are not sums they are not included.

-module( problem34 ).
-export( [main/0] ).

main()
->
    Digit_Map = build_factorial_digit_map(),
    List = sum_digit_factorials_equals_N( Digit_Map ),
    List
.

% Building a small map to reduce the amount of factorial computations we need 
% to do. This is especially useful since we are iterating through the digits 
% of the numbers which we do by converting the integer to a string. 
build_factorial_digit_map()
->
    #{
        "0" => 1,
        "1" => 1,
        "2" => 2,
        "3" => factorial(3),
        "4" => factorial(4),
        "5" => factorial(5),
        "6" => factorial(6),
        "7" => factorial(7),
        "8" => factorial(8),
        "9" => factorial(9)
    }
.

factorial( N )      -> factorial( N, 1 ).
factorial( 1, Acc ) -> Acc;
factorial( N, Acc ) -> factorial( N-1, Acc*N ).

sum_digit_factorials_equals_N( Digit_Map )
->
    sum_digit_factorials_equals_N( Digit_Map, 10, 40586, [] )
.
sum_digit_factorials_equals_N( _Digit_Map, Current, Max, List) when Current >= Max
->
    List
;
sum_digit_factorials_equals_N( Digit_Map, Current, Max, List )
->
    Digit_Sum = sum_digit_factorials( Digit_Map, Current ),

    case Digit_Sum == Current of
        true
        ->
            sum_digit_factorials_equals_N( Digit_Map, Current + 1, Max, [Current | List] )
        ;
        false
        ->
             sum_digit_factorials_equals_N( Digit_Map, Current + 1, Max, List )
    end
.

sum_digit_factorials( Digit_Map, N )
->
    sum_digit_factorials( Digit_Map, integer_to_list(N), 0 )
.
sum_digit_factorials( Digit_Map, [Digit|Rest], Sum )
->
    sum_digit_factorials( Digit_Map, Rest, Sum + maps:get([Digit], Digit_Map) )
;
sum_digit_factorials( _Digit_Map, [], Sum )
->
    Sum
.