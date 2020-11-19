% Problem link: https://projecteuler.net/problem=13


% Problem Statement:
% 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

% What is the sum of the digits of the number 2^1000?

-module(problem16).
-export([main/2]).

main( Base, Exponent )
->
    Number = exp( Base, Exponent ),
    List = integer_to_list( Number ),
    sum_digits( List )
.

sum_digits( List ) -> sum_digits( List, 0 ).
sum_digits( [Digit | List], Acc ) 
-> 
    sum_digits( List, Acc+list_to_integer([Digit]) )
;
sum_digits( [], Acc) 
-> 
    Acc
.

% ----------------------------------------------------------
% Square-Multiply Algorithm for fast integer exponentiation
% ----------------------------------------------------------
exp( B, E ) when E > 3 -> lexp( B, split_exp(E, []), B );
exp( B, 3 )            -> B*B*B;
exp( B, 2 )            -> B*B;
exp( B, 1 )            -> B;
exp( _, 0 )            -> 1.

 

lexp( B, [sqr|L], Acc ) -> lexp( B, L, Acc*Acc );
lexp( B, [_|L], Acc )   -> lexp( B, L, Acc*B );
lexp( _, _, Acc )       -> Acc.


split_exp( E, L ) when E < 2    -> L;
split_exp( E, L )
->
    case ( E rem 2 == 0 ) of
         true   -> split_exp( E div 2, [sqr|L] );
         false  -> split_exp( E - 1, [multiply|L] )
    end
.