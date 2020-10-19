% Problem link: https://projecteuler.net/problem=3


% Problem statement:
    
%     The prime factors of 13195 are 5, 7, 13 and 29.
% 
%     What is the largest prime factor of the number 600851475143 ?

% Note: Prime Factors is currently a list. Replace with a set if your only 
% interest is in unique prime factors. 

% Note: Function currently remembers all prime factors. Can change this to 
% look for only the largest factor. 

-module(problem3).
-export([main/1]).

main(N) ->
    P = largest_prime_factor(N),
    P.

largest_prime_factor(N) ->
    Prime_Factors = prime_factors_of_N(N), 
    Prime_Factors.

prime_factors_of_N(N) ->
    Current_Divisor = 2,
    Prime_Factors = [],
    Current_Quotient = N,
    Max_Factor = N,
    prime_factors_of_N(Prime_Factors, 
                       Current_Divisor, 
                       Current_Quotient, 
                       Max_Factor).

% Naive Base Case: If the Current_Quotient is 1, we are done, return the list  
% of Prime Factors. 
prime_factors_of_N(Prime_Factors, 
                   _Current_Divisor, 
                   1, 
                   _Max_Factor) 
->
    Prime_Factors;
% KEY INSIGHT: For any number N, there is at most 1 prime factor greater than 
%              √N. We can use this fact to prematurely stop the algorithm if 
%              we have found the largest prime factor already!
% Alternative Base Case: If the current divisor is greater than the square 
% root of the current quotient, then it is the largest prime factor.
prime_factors_of_N(Prime_Factors, 
                   Current_Divisor, 
                   Current_Quotient, 
                   Max_Factor) 
when Current_Divisor > Max_Factor 
andalso Current_Quotient rem Current_Divisor =:= 0 ->
    [Current_Divisor | Prime_Factors];
% The first number we will examine is 2. This is the only place where we 
% increment the divisor by 1 (to 3), after which we increment it by two. 
prime_factors_of_N(Prime_Factors, 
                   2, 
                   Current_Quotient, 
                   Max_Factor)
->
    if
        % Current Quotient is even
        Current_Quotient rem 2 =:= 0 ->
            prime_factors_of_N([2 | Prime_Factors], 
                               2, 
                               Current_Quotient div 2,
                               math:sqrt(Current_Quotient div 2));
        % Current Quotient is odd
        Current_Quotient rem 2 =/= 0 ->
            prime_factors_of_N(Prime_Factors, 
                               3, 
                               Current_Quotient,
                               Max_Factor);
        true ->
            io:format("This shouldn't be seen! ~n")
    end;

% If the current divisor divides the current quotient, it is a prime factor
prime_factors_of_N(Prime_Factors, 
                   Current_Divisor, 
                   Current_Quotient, 
                   _Max_Factor)
    when Current_Quotient rem Current_Divisor == 0 ->
        prime_factors_of_N([Current_Divisor | Prime_Factors],
                           Current_Divisor,
                           Current_Quotient div Current_Divisor,
                           math:sqrt(Current_Quotient div Current_Divisor));
% If the current divisor does not divide the current quotient, increment 
% divisor by 2. 
prime_factors_of_N(Prime_Factors, 
                   Current_Divisor, 
                   Current_Quotient, 
                   Max_Factor)
    when Current_Quotient rem Current_Divisor /= 0 ->
        prime_factors_of_N(Prime_Factors,
                           Current_Divisor + 2,
                           Current_Quotient,
                           Max_Factor).
