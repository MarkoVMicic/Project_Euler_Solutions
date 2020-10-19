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

largest_prime_factor(M) ->
    Prime_Factors = prime_factors_to_N(M), 
    Prime_Factors.

prime_factors_to_N(N) ->
    Prime_Factors = [2],
    Int_List = lists:seq(2, N),
    Current_Prime = 2,
    prime_factors_to_N(Prime_Factors,
                       Int_List,
                       [],
                       N,
                       Current_Prime).

% Base Case: All prime factors retrieved from Int_List in reverse order. 
prime_factors_to_N(Prime_Factors,
                   [],
                   [],
                   _N,
                   _Current_Prime)
->
    Prime_Factors;

% If the current integer is evenly divided by the current prime, remove it from the Int_List
prime_factors_to_N(Prime_Factors,
                   [Current_Int | Rest_Int],
                   Temp_List,
                   N,
                   Current_Prime)
when Current_Int rem Current_Prime == 0 ->
    prime_factors_to_N(Prime_Factors,
                       Rest_Int,
                       Temp_List,
                       N,
                       Current_Prime);
% If the current integer is not evenly divided by the current prime, move it to the temp list. 
prime_factors_to_N(Prime_Factors,
                   [Current_Int | Rest_Int],
                   Temp_List,
                   N,
                   Current_Prime)
when Current_Int rem Current_Prime /= 0 ->
    prime_factors_to_N(Prime_Factors,
                       Rest_Int,
                       [Current_Int | Temp_List],
                       N,
                       Current_Prime);
% When we have exhausted the int_list, check if the first value of the new list (i.e. the list with multiples of Current Prime removed) evenly divides N. If not, it is not a prime factor of N, else it is. 
prime_factors_to_N(Prime_Factors,
                   [],
                   Temp_List,
                   N,
                   _Current_Prime)
->
    New_Prime = lists:last(Temp_List),
    New_List = lists:droplast(Temp_List),
    if
        % New_Prime is a prime factor of N
        N rem New_Prime == 0 -> 
            prime_factors_to_N([New_Prime | Prime_Factors],
                                lists:reverse(New_List),
                                [],
                                N,
                                New_Prime);
        % New_Prime is not a factor of N, so call this pattern of the function again. 
        N rem New_Prime /= 0 ->
            prime_factors_to_N(Prime_Factors,
                               [],
                               New_List,
                               N,
                               New_Prime)

    end.

















