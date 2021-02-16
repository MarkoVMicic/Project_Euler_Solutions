% Problem link: https://projecteuler.net/problem=31


% Problem Statement:
% In the United Kingdom the currency is made up of pound (£) and pence (p). 
% There are eight coins in general circulation:

% 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).
% It is possible to make £2 in the following way:
% 
%   1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
% 
% How many different ways can £2 be made using any number of coins?

-module( problem31 ).
-export( [ main/2 ] ).

% TODO: Is there a way to do this with Tail Recursion?
% TODO: Learn to do this with generating functions (i.e. polynomials whose 
%       exponents match the denomination value)

main( Amount, Coin_List )
->
    % Sort and remove duplicates
    Sorted_Coin_List = lists:usort( Coin_List ),
    num_ways_to_create_amount_from_coins( Amount, Sorted_Coin_List )
.

%
% Recursive Algorithm considering one coin at a time. 
% 
% Positive Base Case: In the event that we have an exact match between the 
%                     current coin and the amount we are trying to generate, 
%                     we have exactly 1 way to do this: just use the current 
%                     coin!
% NOTE: The way we've written it here is to check that N == 0. This makes 
%       sense since if N == 0 now, then at the previous call N = Current_Coin (
%       see next function clause)
num_ways_to_create_amount_from_coins( 0, _Coin_List )
-> 
    1
;
% Negative Base Case 1: If N is negative, then we have run into the condition 
%                       where the remaining coins we have at our disposal are 
%                       too large to build the current value, which means we 
%                       have no way of assembling the amount. 
num_ways_to_create_amount_from_coins( N, _Coin_List ) when N < 0
->
    0
;
% Negative Base Case 2: If our Coins List is empty, we obviously cannot create 
%                       any amount with an empty list. 
num_ways_to_create_amount_from_coins( _N, [] )
-> 
    0
;
% Recursion: We split into two parts:
%            (1) Use the current coin to whittle away at the current amount, 
%                keeping the list of coins the same
%            (2) Keep the current amount you're trying to build constant, and 
%                remove one of the coins from consideration.
% (1) Can be thought of as persistently trying to see how many times 
%     Current_Coin can go into N before hitting one of the base cases
% (2) Can be thought of as persistently removing coins from the list and 
%     trying to assemble N using the reduced list of coins. 
% By repeatedly splitting into these two parts, we eventually reduce to one of 
% the three base cases above. 
num_ways_to_create_amount_from_coins( N, [Current_Coin | Rest] = Coin_List ) 
->
    % Part (1)
    num_ways_to_create_amount_from_coins( N - Current_Coin, Coin_List ) + 
    % Part (2) 
    num_ways_to_create_amount_from_coins( N, Rest )
.
