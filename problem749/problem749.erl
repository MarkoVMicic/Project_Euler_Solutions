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
-export( [ main/1,
           is_near_power_sum/1,
           find_starting_k/1,
           sum_digits/1 ] ).

% Note: Number_Digits needs to be >= 2
main( Number_Digits )
->
    % Using s as defined in the problem.
    s( Number_Digits )
.

s( Number_Digits )
->
    % Start at 10, because no single digit number is a near power sum number
    Initial_Value = 10,
    Max_Value = exp(10, Number_Digits ),
    s( Initial_Value, Max_Value, 0 )
.
s( Current_Value, Max_Value, Acc ) when Current_Value >= Max_Value
->
    Acc
;
s( Current_Value, Max_Value, Acc ) when Current_Value rem 100000 == 0
->
    io:fwrite("~p/~p~n",    [Current_Value, Max_Value]),
    case is_near_power_sum( Current_Value ) of
        true
        ->
            s( Current_Value + 1, Max_Value, Acc + Current_Value )
        ;
        false
        ->
            s( Current_Value + 1, Max_Value, Acc )
    end 
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
    K = find_starting_k( Number ),
    is_near_power_sum( Number, K, 0 )
.
is_near_power_sum( Number, K, Previous )
->
    Power_Sum = power_sum( Number, K ),
    case Power_Sum >  Number + 1 orelse
         Power_Sum =< Previous          of
        true
        ->
            % If we have exceeded N+1 with the power sum, then we don't have a 
            % near power sum number. 
            % If the current Power_Sum is less than or equals to the previous 
            % Power_Sum (from a smaller K), then we have a case where our 
            % digits are all 0s or 1s only and therefore the Power_Sum will be 
            % the same for all K, so we need to short-circuit here to prevent 
            % infinite recursion. 
            false
        ;
        false
        ->
            case Power_Sum == Number - 1 orelse 
                 Power_Sum == Number + 1         of
                true
                ->
                    true
                ;
                false
                ->
                    % Try the next positive integer K.
                    is_near_power_sum( Number, K+1, Power_Sum )
            end
    end 
.


find_starting_k( Number )
->
    Sum_Of_Digits = sum_digits( Number ),
    find_starting_k( Number, Sum_Of_Digits, 1, 0 )
.
find_starting_k( Number, Sum_Of_Digits, K, Previous )
->
    Kth_Power_Sum_Digits = exp( Sum_Of_Digits, K ),
    case Kth_Power_Sum_Digits > Number orelse
         Kth_Power_Sum_Digits =< Previous     of
        true
        ->
            % Found the starting K on the previous iteration
            K - 1
        ;
        false
        ->
            find_starting_k( Number, Sum_Of_Digits, K + 1, Kth_Power_Sum_Digits )
    end
.

sum_digits( Number )
->
    sum_digits( integer_to_list(Number), 0 )
.
sum_digits( [Current_Digit | Rest], Acc )
->
    sum_digits( Rest, list_to_integer([Current_Digit]) + Acc )
;
sum_digits( [], Acc )
->
    Acc
.


power_sum( Number, K )
->
    power_sum( integer_to_list(Number), K, 0 )
.
power_sum( [Current_Digit | Rest], K, Acc )
->
    Digit_Power = exp( list_to_integer([Current_Digit]), K ),
    power_sum( Rest, K, Acc+Digit_Power )
;
power_sum( [], _K, Acc )
->
    Acc
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