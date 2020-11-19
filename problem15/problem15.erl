% Problem link: https://projecteuler.net/problem=13


% Problem Statement:

% Starting in the top left corner of a 2×2 grid, and only being able to move 
% to the right and down, there are exactly 6 routes to the bottom right corner.

% .->->    .->      .->      .       .       .
%     |      |        |      |       |       |
%     v      v->      v      v->->   v->     v
%     |        |      |          |     |     |
%     v        v      v->        v     v->   v->->


% How many such routes are there through a 20×20 grid?

-module(problem15).
-export([main/1]).

main( N )
->
    binomial_coefficient( 2*N, N )
.

binomial_coefficient(N, K)
->
    factorial(N) div (factorial(K) * factorial(N-K))
.

factorial( N )      -> factorial( N, 1 ).
factorial( 1, Acc ) -> Acc;
factorial( N, Acc ) -> factorial( N-1, Acc*N ).

% Using some simple combinatorics, suppose we let each move to the right be 
% denoted R, and each move downwards be denoted D.
% Since we are in an NxN grid, we must have exactly N R moves and N D moves, 
% for a total of 2*N moves.  
% Thus the question becomes how many different combinations of N Rs and N Ds 
% can we create, which is essentially just (2*N choose N), where (N choose K) 
% is defined as N!/(K!(N-K)!)