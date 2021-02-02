% Problem link: https://projecteuler.net/problem=48


% Problem Statement:
% The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

% Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-module( problem48 ).
-export( [main/2] ).

main( N, K )
->
    List = lists:seq( 1, N ),
    Sum = sum_of_exponents( List ),
    String = find_last_K_digits( Sum, K ),
    Answer = list_to_integer( String ),
    Answer
.

sum_of_exponents( List ) -> sum_of_exponents( List, 0 ).
sum_of_exponents( [Current | Rest], Sum )
->
    sum_of_exponents( Rest, Sum+exp(Current, Current) )
;
sum_of_exponents( [], Sum)
->
    Sum
.

find_last_K_digits( Sum, K )
->
    String = integer_to_list( Sum ),
    R_String = lists:reverse( String ),
    {R_Substring, _} = lists:split( K, R_String ),
    lists:reverse( R_Substring )
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
