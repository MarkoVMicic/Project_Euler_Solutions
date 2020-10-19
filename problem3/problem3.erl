% Problem link: https://projecteuler.net/problem=3


% Problem statement:
    
%     The prime factors of 13195 are 5, 7, 13 and 29.
% 
%     What is the largest prime factor of the number 600851475143 ?

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
    prime_factors_of_N(Prime_Factors, Current_Divisor, Current_Quotient).

% Base Case: If the Current_Quotient is 1, we are done, return the list of 
% Prime Factors. 
prime_factors_of_N(Prime_Factors, _Current_Divisor, 1) 
->
    Prime_Factors;
% If the current divisor divides the current quotient, it is a prime factor
prime_factors_of_N(Prime_Factors, Current_Divisor, Current_Quotient)
    when Current_Quotient rem Current_Divisor == 0 ->
        prime_factors_of_N([Current_Divisor | Prime_Factors],
                           Current_Divisor,
                           Current_Quotient div Current_Divisor);
% If the current divisor does not divide the current quotient, increment 
% divisor by 1. 
prime_factors_of_N(Prime_Factors, Current_Divisor, Current_Quotient)
    when Current_Quotient rem Current_Divisor /= 0 ->
        prime_factors_of_N(Prime_Factors,
                           Current_Divisor + 1,
                           Current_Quotient).
















