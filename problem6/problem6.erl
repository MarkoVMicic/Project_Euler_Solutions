% Problem link: https://projecteuler.net/problem=6

% The sum of the squares of the first ten natural numbers is,

%     1^2 + 2^2 + ... + 10^2 = 385

% The square of the sum of the first ten natural numbers is,

%     (1 + 2 + ... + 10)^2 = 3025

% Hence the difference between the sum of the squares of the first ten natural 
% numbers and the square of the sum is 3025 - 385 = 2640.

% Find the difference between the sum of the squares of the first one hundred 
% natural numbers and the square of the sum.

-module(problem6).
-export([naive_main/1, fast_main/1]).

% Brute force. 
naive_main( N )
->
    SumSquares = naive_sum_of_squares( N ),
    SquareSum = naive_square_of_sum( N ),
    SquareSum - SumSquares
.

fast_main( N )
->
    SumSquares = fast_sum_of_squares( N ),
    SquareSum = fast_square_of_sum( N ),
    SquareSum - SumSquares
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

naive_sum_of_squares( N ) -> naive_sum_of_squares( lists:seq(1, N), 0 ).
naive_sum_of_squares( [H|L], Acc )
->
    naive_sum_of_squares( L, Acc + (H*H) ) 
;
naive_sum_of_squares( [], Acc ) 
->
    Acc
.

naive_square_of_sum( N ) -> naive_square_of_sum( lists:seq(1,N), 0 ).
naive_square_of_sum( [H|L], Acc )
->
    naive_square_of_sum( L, Acc+H )
;
naive_square_of_sum( [], Acc )
->
    Acc*Acc
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% See bottom of file for explanation. 
fast_sum_of_squares( N ) -> ( N * ( 2*N + 1 ) * ( N+1 ) ) div 6.

% uses famous formula ∑_{i=1}^n i = (n*(n+1))/2
fast_square_of_sum( N ) 
->
    Sum = ( N * ( N+1 ) ) div 2,
    Sum*Sum
.

% Suppose we wish to compute some function f(N) = ∑_{i=0}^N i^2. 
% To do this, let us examine first the formula 
%     g(N) = ∑_{i=0}^N i = (n(n+1))/2
% (Note that in both f and g, the sum begins from 0. This is useful as a first 
% term of 0 will help us discover the formula for f).
% We note here that g is a second degree polynomial. Since f deals with 
% squaring the individual terms of g, we might start from thinking of f as 
% being a third degree polynomial in N:
%     f(N) = AN^3 + BN^2 + CN + D.
% We can then use the first 4 values of N to write down

%     f(0) =                 D = 0
%     f(1) = A   + B  + C +  D = 1
%     f(2) = 8A  + 4B + 2C + D = 5 
%     f(3) = 27A + 9B + 3C + D = 14

% This is a system of 4 equations in 4 unknowns, which we can solve to see:
%     D = 0, C = 1/6, B = 1/2, A = 1/3
% Thus
%     f(N) = N^3/3  + N^2/2  + N/6
%          = 2N^3/6 + 3N^2/6 + N/6
%          = (N(2N^2 + 3N + 1))/6
%          = (N(2N + 1)(N+1))/6

% Then, we can check that this is true for all N using induction. Based on our 
% derivation above, we already know that it is true for our base case, so 
% assume that f(N) is true for N. Now check for N+1.

%     ∑_{i=0}^{N+1} i^2  = ∑_{i=0}^N i^2 + (N+1)^2
%                        = f(N) + (N+1)^2
%                        = (N(2N + 1)(N+1))/6 + (N+1)^2
%                        = (N(2N + 1)(N+1))/6 + 6(N+1)^2/6
%                        = ((2N^2 + N)(N+1) + 6(N+1)^2)/6
%                        = (2N^3 + 3N^2 + N + 6N^2 + 12N + 6)/6
%                        = (2N^3 + 9N^2 + 13N + 6N^2 + 6)/6
%     multiply by 6 to avoid writing fractions all the time:
%     6∑_{i=0}^{N+1} i^2 = 2N^3 + 9N^2 + 13N + 6N^2 + 6
%     and
%     6f(N+1)            = (N+1)(2(N+1) + 1)(N+1+1)
%                        = (N+1)(2N+2 + 1)(N+2)
%                        = (N+1)(2N+3)(N+2)
%                        = (2N^2 + 5N + 3)(N+2)
%                        = 2N^3 + 4N^2 + 5N^2 + 10N + 3N + 6
%                        = 2N^3 + 9N^2 + 13N + 6N^2 + 6
% Since both are equal, we have proven the formula by induction. 
