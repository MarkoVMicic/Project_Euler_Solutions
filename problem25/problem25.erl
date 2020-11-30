% Problem link: https://projecteuler.net/problem=25


% Problem Statement:
% The Fibonacci sequence is defined by the recurrence relation:

% Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
% Hence the first 12 terms will be:

% F1 = 1
% F2 = 1
% F3 = 2
% F4 = 3
% F5 = 5
% F6 = 8
% F7 = 13
% F8 = 21
% F9 = 34
% F10 = 55
% F11 = 89
% F12 = 144
% The 12th term, F12, is the first term to contain three digits.

% What is the index of the first term in the Fibonacci sequence to contain 
% 1000 digits?

-module(problem25).
-export([main/1]). 

main( N )
->
    fibonacci_digits( N )
.

fibonacci_digits( N ) -> fibonacci_digits( N, 2, 1, 1, 0 ).
fibonacci_digits( N, 
                  Index, 
                  F_n_minus_1, 
                  F_n_minus_2, 
                  Num_Digits ) when Num_Digits == N
->
    {Index, F_n_minus_1}
;
fibonacci_digits( N, 
                  Index, 
                  F_n_minus_1, 
                  F_n_minus_2, 
                  _Num_Digits )
->
    F_n = F_n_minus_1 + F_n_minus_2,
    fibonacci_digits( N,
                      Index+1,
                      F_n,
                      F_n_minus_1,
                      length(integer_to_list(F_n)) )
.
