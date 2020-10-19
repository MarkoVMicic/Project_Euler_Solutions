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
    Primes = primes_to_N(N),
    Primes.

primes_to_N(N) ->
    Int_List = lists:seq(2, N),
    primes_to_N(Int_List, N).

primes_to_N(Int_List, _N) ->
    Primes_List = [2],
    Temp_List = [],
    primes_to_N(Primes_List, Int_List, Temp_List).

% Base Case: All primes retrieved from Int_List in reverse order.  
primes_to_N(Primes_List,
            [],
            []) 
->
    Primes_List;
% For current prime, all values in Int_List checked. Multiples of current prime already stripped from Int_List and NOT inserted into Temp List. At this stage, we want to reverse Temp_List, take the first value, put it in the list of primes, and then put the rest as the new Int_List, passing an empty list to Temp_List parameter. 
primes_to_N(Primes,
            [],
            Temp_List)
->
    New_List = lists:reverse(Temp_List),
    [New_Prime | Int_List] = New_List,
    primes_to_N([New_Prime | Primes],
                Int_List,
                []);
% If the current prime number evenly divides the current int, then we clearly have a composite number, so we simply remove it from the list without adding it to the temp list. 
primes_to_N([Current_Prime | Primes_Rest],
            [Current_Int | Int_Rest],
            Temp_List)
when Current_Int rem Current_Prime == 0 ->
    primes_to_N([Current_Prime | Primes_Rest],
                Int_Rest,
                Temp_List);

% If the current prime number does not evenly divide the current int, then we move the value from the int list to the temp list. 
primes_to_N([Current_Prime | Primes_Rest],
            [Current_Int | Int_Rest],
            Temp_List)
->
    primes_to_N([Current_Prime | Primes_Rest],
                Int_Rest,
                [Current_Int | Temp_List]).

