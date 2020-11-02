% Problem link: https://projecteuler.net/problem=5


% Problem statement:
    
% 2520 is the smallest number that can be divided by each of the numbers from 
% 1 to 10 without any remainder.

% What is the smallest positive number that is evenly divisible by all of the 
% numbers from 1 to 20?

-module(problem5).
-export([main/1]).

main(N)
->
    find_number_evenly_divisble_up_to_n(1, 1, N)
.

% Naive solution: Iterate through all divisors until N, and increment your 
% number by 1 everytime you find something that isn't evenly divisible. 

find_number_evenly_divisble_up_to_n(N, Number, N) -> Number;
find_number_evenly_divisble_up_to_n(Factor, Number, N)
when Number rem Factor == 0
->
    find_number_evenly_divisble_up_to_n(Factor+1, Number, N);
find_number_evenly_divisble_up_to_n(_Factor, Number, N)
->
    find_number_evenly_divisble_up_to_n(1, Number+1, N).
