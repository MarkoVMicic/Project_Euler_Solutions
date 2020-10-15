% Problem link: https://projecteuler.net/problem=2


% Problem statement:
    
%     By considering the terms in the Fibonacci sequence whose values do not 
%     exceed four million, find the sum of the even-valued terms.

-module(problem2).
-export([main/1]).

main(MaxValue) ->
    Answer = sum_even_fibonacci(MaxValue),
    Answer.

sum_even_fibonacci(MaxValue) ->
    Index = 0,
    Acc = 0,
    F_t = 1,
    F_tminus1 = 1,
    sum_even_fibonacci(Index, 
                       Acc, 
                       F_t, 
                       F_tminus1, 
                       MaxValue).

sum_even_fibonacci(_, Acc, F_t, _, MaxValue) 
    when F_t > MaxValue ->
    Acc;
sum_even_fibonacci(Index, Acc, F_t, F_tminus1, MaxValue) 
    when Index rem 3 == 0 ->
    sum_even_fibonacci(Index + 1, 
                       Acc + F_t + F_tminus1, 
                       F_t + F_tminus1, 
                       F_t, 
                       MaxValue);
sum_even_fibonacci(Index, Acc, F_t, F_tminus1, MaxValue) ->
    sum_even_fibonacci(Index + 1, 
                       Acc, 
                       F_t + F_tminus1, 
                       F_t, 
                       MaxValue).

