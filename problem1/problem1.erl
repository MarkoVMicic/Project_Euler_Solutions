-module(problem1).
-export([naive_main/1, fast_main/1]).

naive_main(Max_value) ->
    Naive = naive(Max_value),
    Naive.

fast_main(Max_value) ->
    Fast = fast(Max_value),
    Fast.

naive(Max_value) ->
    Naive = naive(0, Max_value-1),
    Naive.
naive(Acc, 0) -> Acc;
naive(Acc, Current_value) when 
    Current_value rem 3 == 0; Current_value rem 5 == 0 ->
    naive(Acc+Current_value, Current_value - 1);
naive(Acc, Current_value) ->
    naive(Acc, Current_value - 1).

fast(Max_value) ->
    Fast = sum_divisible_by(3, Max_value) + sum_divisible_by(5, Max_value) - sum_divisible_by(15, Max_value),
    Fast.

sum_divisible_by(Divisor, Value) ->
    M = (Value-1) div Divisor,
    Sum = Divisor * ((M*(M+1))/2),
    Sum.
