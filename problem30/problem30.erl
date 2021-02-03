% Problem link: https://projecteuler.net/problem=30


% Problem Statement:
% Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

% 1634 = 1^4 + 6^4 + 3^4 + 4^4
% 8208 = 8^4 + 2^4 + 0^4 + 8^4
% 9474 = 9^4 + 4^4 + 7^4 + 4^4
% As 1 = 1^4 is not a sum it is not included.

% The sum of these numbers is 1634 + 8208 + 9474 = 19316.

% Find the sum of all the numbers that can be written as the sum of fifth 
% powers of their digits.

-module( problem30 ).
-export( [ main/1,
           find_max_value/1,
           exp/2 ] ).

% This program finds the sum of numbers that are equal to the sum of nth 
% powers of their digits. 
main( N )
->
    Max_Value = find_max_value( N ),
    List = lists:seq( 2, Max_Value ),
    Numbers = find_numbers( List, N ),
    lists:sum( Numbers )
.

% See comment at end of file for explanation of this max_value computation. 
find_max_value( N )
->
    Max_Digit_Value = exp( 9, N ),
    find_max_value( Max_Digit_Value, 1 )
.
find_max_value( Max_Digit_Value, Num_Digits ) 
->

    Max_Number = Num_Digits * Max_Digit_Value,
    case length(integer_to_list(Max_Number)) < Num_Digits of
        true    ->  (Num_Digits - 1) * Max_Digit_Value;
        false   ->  find_max_value( Max_Digit_Value, Num_Digits + 1)
    end
.


find_numbers( List, N )
->
    find_numbers( List, N, [] )
.
find_numbers( [Current | Rest], N, Numbers )
->
    case is_digit_power( Current, N ) of
        true  -> find_numbers( Rest, N, [Current | Numbers] );
        false -> find_numbers( Rest, N, Numbers )
    end
;
find_numbers( [], _N, Numbers )
->
    lists:reverse( Numbers )
.


is_digit_power( Number, Exponent )
->
    String = integer_to_list( Number ),
    sum_digit_power( String, Exponent ) == Number
.

sum_digit_power( String, Exponent ) 
->
    sum_digit_power( String, Exponent, 0 )
.
sum_digit_power( [Current | Rest], Exponent, Sum )
->
    Digit = list_to_integer( [Current] ),
    sum_digit_power( Rest, Exponent, Sum + exp(Digit, Exponent) )
;
sum_digit_power( [], _Exponent, Sum )
->
    Sum
.


% ----------------------------------------------------------
% Square-Multiply Algorithm for fast integer exponentiation
% ----------------------------------------------------------
exp( B, E ) when E > 3 -> lexp( B, split_exp(E, []), B );
exp( B, 3 )            -> B*B*B;
exp( B, 2 )            -> B*B;
exp( B, 1 )            -> B;
exp( _, 0 )            -> 1.

 

lexp( B, [sqr|L], Acc ) -> lexp( B, L, Acc*Acc );
lexp( B, [_|L], Acc )   -> lexp( B, L, Acc*B );
lexp( _, _, Acc )       -> Acc.


split_exp( E, L ) when E < 2    -> L;
split_exp( E, L )
->
    case ( E rem 2 == 0 ) of
         true   -> split_exp( E div 2, [sqr|L] );
         false  -> split_exp( E - 1, [multiply|L] )
    end
.
% ------------------------------------
% Explanation of Max_Value computation
% ------------------------------------

% The sum of the nth digits of powers is maximized when every digit is 9. Our 
% goal here is to find the maximum number of digits allowed that can be 
% expressed as the sum of the nth power of its digits. To do this, we simply 
% see if the maximum possible value (9999....) when having its digits raised 
% to the nth power and then summed, is less than the number of digits in that 
% particular number. 
% As a worked example (applicable to the question at hand), consider N = 5. 
% Then 
%   For 1 digit:    1 * 9^N = 1 * 9^5 = 59049;     Num_Digits = 5 >= 1
%   For 2 digits:   2 * 9^N = 2 * 9^5 = 118098;    Num_Digits = 6 >= 2
%   For 3 digits:   3 * 9^N = 3 * 9^5 = 177147;    Num_Digits = 6 >= 3
%   For 4 digits:   4 * 9^N = 4 * 9^5 = 236196;    Num_Digits = 6 >= 4
%   For 5 digits:   5 * 9^N = 5 * 9^5 = 295245;    Num_Digits = 6 >= 5
%   For 6 digits:   6 * 9^N = 6 * 9^5 = 354294;    Num_Digits = 6 >= 6
%   For 7 digits:   7 * 9^N = 7 * 9^5 = 413343;    Num_Digits = 6 <  7!!
%
% Therefore the maximum possible value is 354294 when N=5
%
% A second example: Consider N = 6. 
% Then
%   For 1 digit:    1 * 9^N = 1 * 9^6 = 531441;     Num_Digits = 6 >= 1
%   For 2 digits:   2 * 9^N = 2 * 9^6 = 1062882;    Num_Digits = 7 >= 2
%   For 3 digits:   3 * 9^N = 3 * 9^6 = 1594323;    Num_Digits = 7 >= 3
%   For 4 digits:   4 * 9^N = 4 * 9^6 = 2125764;    Num_Digits = 7 >= 4
%   For 5 digits:   5 * 9^N = 5 * 9^6 = 2657205;    Num_Digits = 7 >= 5
%   For 6 digits:   6 * 9^N = 6 * 9^6 = 3188646;    Num_Digits = 7 >= 6
%   For 7 digits:   7 * 9^N = 7 * 9^6 = 3720087;    Num_Digits = 7 >= 7
%   For 8 digits:   8 * 9^N = 8 * 9^6 = 4251528;    Num_Digits = 7 <  8!!
% 
% Therefore the maximum possible value is 3720087 when N=6
