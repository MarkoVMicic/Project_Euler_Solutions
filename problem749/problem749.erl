% Problem link: https://projecteuler.net/pelroblem=749


% Problem statement:
% A positive integer, N , is a near power sum if there exists a positive 
% integer, K , such that the sum of the Kth powers of the digits in its 
% decimal representation is equal to either N-1 or N+1 . For example 35 is a 
% near power sum number because 3^2 + 5^2 = 34.

% Define s(N) to be the sum of all near power sum numbers of N digits or less. 
% Then s(2) = 110 and s(6) = 2562701.

% Find S(16).

-module( problem749 ).
-export( [ main/1 ] ).

% Note: Number_Digits needs to be >= 2
main( Number_Digits )
->
    % Using s as defined in the problem.
    s( Number_Digits )
.

s( Number_Digits )
->
    % Start at 10, because no single digit number is a near power sum numbers
    Initial_Value = 10,
    Max_Value = exp(10, Number_Digits )
    s( Initial_Value, Max_Value, 0 )
.
s( Current_Value, Max_Value, Acc ) when Current_Value >= Max_Value
->
    Acc
;
s( Current_Value, Max_Value, Acc )
->
    case is_near_power_sum( Current_Value ) of
        true
        ->
            s( Current_Value + 1, Max_Value, Acc + Current_Value )
        ;
        false
        ->
            s( Current_Value + 1, Max_Value, Acc )
    end 
.


is_near_power_sum( Number )
->
    false
.



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