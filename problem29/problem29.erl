
% Problem link: https://projecteuler.net/problem=29


% Problem Statement:
% Consider all integer combinations of ab for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:

% 2^2=4,  2^3=8,   2^4=16,  2^5=32
% 3^2=9,  3^3=27,  3^4=81,  3^5=243
% 4^2=16, 4^3=64,  4^4=256, 4^5=1024
% 5^2=25, 5^3=125, 5^4=625, 5^5=3125

% If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:

% 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

% How many distinct terms are in the sequence generated by ab for 2 ≤ a ≤ 100 
% and 2 ≤ b ≤ 100?

-module(problem29).
-export( [main/4] ).

main( Min_A, Max_A, Min_B, Max_B )
->
    List = generate_list( Min_A, Max_A, Min_B, Max_B  ),
    length( lists:usort(List) )
.

generate_list( Min_A, Max_A, Min_B, Max_B )
->
    generate_list( Min_A, Max_A, Min_B, Max_B, Min_A, Min_B, [] )
.
generate_list( _Min_A, Max_A, 
               _Min_B, _Max_B, 
               Current_A, _Current_B, 
               List )   when Current_A > Max_A
->
    List
;
generate_list( Min_A, Max_A, 
               Min_B, Max_B, 
               Current_A, Current_B, 
               List )   when Current_B > Max_B
->
    generate_list( Min_A, Max_A, 
                   Min_B, Max_B, 
                   Current_A+1, Min_B, 
                   List )
;
generate_list( Min_A, Max_A, 
               Min_B, Max_B, 
               Current_A, Current_B, 
               List )
->
    generate_list( Min_A, Max_A, 
                   Min_B, Max_B, 
                   Current_A, Current_B+1, 
                   [exp(Current_A, Current_B) | List] )
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