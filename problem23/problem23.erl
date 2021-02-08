
% Problem link: https://projecteuler.net/problem=23


% Problem Statement:
% A perfect number is a number for which the sum of its proper divisors is 
% exactly equal to the number. For example, the sum of the proper divisors of 
% 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

% A number n is called deficient if the sum of its proper divisors is less 
% than n and it is called abundant if this sum exceeds n.

% As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest 
% number that can be written as the sum of two abundant numbers is 24. By 
% mathematical analysis, it can be shown that all integers greater than 28123 
% can be written as the sum of two abundant numbers. However, this upper limit 
% cannot be reduced any further by analysis even though it is known that the 
% greatest number that cannot be expressed as the sum of two abundant numbers 
% is less than this limit.


% Find the sum of all the positive integers which cannot be written as the sum 
% of two abundant numbers.

-module( problem23 ).
-export( [ main/1,
           find_abundant_numbers/2 ] ).

main( Upper_Limit )
->
    Abundant_Numbers = find_abundant_numbers( 12, Upper_Limit ),
    find_sum( 24, Upper_Limit, Abundant_Numbers )
.

find_abundant_numbers( Start, Limit )
->
    find_abundant_numbers( Start, Limit, [] )
.
find_abundant_numbers( Current, Limit, Abundant_Numbers ) when Current > Limit
->
    lists:reverse( Abundant_Numbers )
;
find_abundant_numbers( Current, Limit, Abundant_Numbers )
-> 
    case is_abundant( Current ) of
        true    ->  find_abundant_numbers( Current + 1, Limit, [Current|Abundant_Numbers] );
        false   ->  find_abundant_numbers( Current + 1, Limit, Abundant_Numbers )
    end
.

is_abundant( N )
->
    Factors = find_factors( N ),
    lists:sum( Factors ) > N
.

find_factors( N )
->
    find_factors( N, math:sqrt(N), lists:seq(1, N div 2), [] )
.
find_factors( N, Sqrt_N, [Current | Rest], Factors ) when N rem Current == 0
->
    find_factors( N, Sqrt_N, Rest, [Current | Factors] )
;
% Found last proper factor if above Sqrt_N!
find_factors( N, Sqrt_N, [Current | _Rest], Factors ) when N rem Current == 0 andalso
                                                          Current > Sqrt_N
->
    [Current | Factors]
;
find_factors( N, Sqrt_N, [_Current | Rest], Factors )
->
    find_factors( N, Sqrt_N, Rest, Factors )
;
find_factors( _N, _Sqrt_N, [], Factors )
->
    Factors
.

find_sum( Current, Upper_Limit, Abundant_Numbers )
->
    find_sum( Current, Upper_Limit, Abundant_Numbers, lists:sum(lists:seq(1,23)) )
.
find_sum( Current, Upper_Limit, _Abundant_Numbers, Sum ) when Current > Upper_Limit
->
    Sum
;
find_sum( Current, Upper_Limit, Abundant_Numbers, Sum )
->
    case is_sum_of_two_abundant_numbers( Current, Abundant_Numbers ) of
        true    
        -> 
            find_sum( Current + 1, Upper_Limit, Abundant_Numbers, Sum  )
        ;
        false   
        -> 
            find_sum( Current + 1, Upper_Limit, Abundant_Numbers, Sum + Current )
    end
.

is_sum_of_two_abundant_numbers( Current, [Abundant_Number | _Rest] ) when Abundant_Number >= Current
->
    false
;
is_sum_of_two_abundant_numbers( Current, [Abundant_Number | Rest] )
->
    Difference = Current - Abundant_Number,
    case lists:member( Difference, [Abundant_Number|Rest] ) of
        true    
        -> 
            true
        ;
        false   
        -> 
            is_sum_of_two_abundant_numbers( Current, Rest )
    end
;
is_sum_of_two_abundant_numbers( _Current, [] )
->
    false
.
