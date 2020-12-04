% Problem link: https://projecteuler.net/problem=21


% Problem Statement:

% Let d(n) be defined as the sum of proper divisors of n (numbers less than n 
% which divide evenly into n).
% If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and 
% each of a and b are called amicable numbers.

% For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 
% 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 
% 71 and 142; so d(284) = 220.

% Evaluate the sum of all the amicable numbers under 10000.

-module( problem21 ).
-export( [naive_main/1, fast_main/1] ).

naive_main( N )
->
    sum_amicable_numbers( N )
.

fast_main( N )
->
    sum_amicable_numbers_fast( N )
.

% We begin from 2 because 1 has no proper divisors according to the definition 
% of the problem statement. 
sum_amicable_numbers( N ) -> sum_amicable_numbers( N, 2, 0 ).
sum_amicable_numbers( N, Current_Number, Acc) when Current_Number == N
->
    Acc
;
sum_amicable_numbers( N, Current_Number, Acc )
->
    Sum_Divisors = sum_divisors( Current_Number ),
    % Note that according to the problem, a number cannot be amicable with 
    % itself, so we check for that equality here.
    case sum_divisors( Sum_Divisors ) == Current_Number andalso 
         Sum_Divisors =/= Current_Number                of
        true    
        ->  
            io:fwrite("~p, ~p are amicable numbers.~n", [Current_Number, Sum_Divisors]),
            sum_amicable_numbers( N, Current_Number+1, Acc+Current_Number );
        false
        ->
            sum_amicable_numbers( N, Current_Number+1, Acc )
    end
.

sum_divisors( N ) -> sum_divisors( N, 1, 0 ).
sum_divisors( N, Current_Number, Acc ) when Current_Number == N
->
    Acc
;
sum_divisors( N, Current_Number, Acc ) when N rem Current_Number == 0
->
    sum_divisors( N, Current_Number+1, Acc+Current_Number )
;
sum_divisors( N, Current_Number, Acc )
->
    sum_divisors( N, Current_Number+1, Acc )
.

% This cuts the number of times we evaluate sum_divisors in half by filtering 
% for when sum_divisors( N ) > N. Makes the code uglier. 
sum_amicable_numbers_fast( N ) -> sum_amicable_numbers_fast( N, 2, 0 ).
sum_amicable_numbers_fast( N, Current_Number, Acc ) when Current_Number == N
->
    Acc
;
sum_amicable_numbers_fast(N, Current_Number, Acc )
->
    Sum_Divisors = sum_divisors_fast( Current_Number ),
    if 
        Sum_Divisors > Current_Number 
        ->
            case sum_divisors_fast( Sum_Divisors ) == Current_Number andalso 
                 Sum_Divisors =/= Current_Number                of
            true    
            ->  
                io:fwrite("~p, ~p are amicable numbers.~n", [Current_Number, Sum_Divisors]),
                sum_amicable_numbers_fast( N, 
                                           Current_Number+1, 
                                           Acc+Current_Number+Sum_Divisors );
            false
            ->
                sum_amicable_numbers_fast( N, 
                                           Current_Number+1, 
                                           Acc )
            end
        ;
        true
        ->
            sum_amicable_numbers_fast( N, 
                                       Current_Number+1,
                                       Acc )
    end
.

sum_divisors_fast( 2 ) -> 1;
sum_divisors_fast( 3 ) -> 1;
sum_divisors_fast( N ) 
-> 
    Sqrt_N = trunc( math:sqrt(N) ),
    case Sqrt_N * Sqrt_N == N of
        % If n is a perfect square, then we need to add its square root to the 
        % sum of proper divisors since it will be otherwise missed by the algorithm 
        true
        ->
            sum_divisors_fast( N, Sqrt_N, 1, Sqrt_N)
        ;
        false
        ->
            sum_divisors_fast( N, Sqrt_N, 1, 0)            
    end
.
sum_divisors_fast( N, 
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when Current_Number >= Sqrt_N
->
    Acc - N
;
% If N is odd, it cannot have even divisors, so we can increment 
% Current_Number by 2. 
sum_divisors_fast( N,
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when N rem 2 == 1,
                              N rem Current_Number == 0
->
    sum_divisors_fast( N,
                       Sqrt_N, 
                       Current_Number + 2,
                       Acc + Current_Number + (N div Current_Number) )
;
sum_divisors_fast( N,
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when N rem 2 == 1
->
    sum_divisors_fast( N,
                       Sqrt_N, 
                       Current_Number + 2,
                       Acc )
;
% If N is even, then it can have both even and odd divisors, so we can only 
% increment Current_Number by 1. 
sum_divisors_fast( N,
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when N rem 2 == 0,
                              N rem Current_Number == 0
->
    sum_divisors_fast( N,
                       Sqrt_N, 
                       Current_Number + 1,
                       Acc + Current_Number + (N div Current_Number) )
;
sum_divisors_fast( N,
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when N rem 2 == 0
->
    sum_divisors_fast( N,
                       Sqrt_N, 
                       Current_Number + 1,
                       Acc )
.
