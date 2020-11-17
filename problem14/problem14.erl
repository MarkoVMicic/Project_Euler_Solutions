% Problem link: https://projecteuler.net/problem=13


% Problem Statement:


% The following iterative sequence is defined for the set of positive integers:

% n → n/2 (n is even)
% n → 3n + 1 (n is odd)

% Using the rule above and starting with 13, we generate the following 
% sequence:

% 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
% It can be seen that this sequence (starting at 13 and finishing at 1) 
% contains 10 terms. Although it has not been proved yet Collatz Problem), it 
% is thought that all starting numbers finish at 1.

% Which starting number, under one million, produces the longest chain?

% NOTE: Once the chain starts the terms are allowed to go above one million.

-module( problem14 ).
-export( [main/1] ).

main( N )
->
    longest_chain_up_to_N( N )
.

longest_chain_up_to_N( N ) 
-> 
    longest_chain_up_to_N( N, 1, 1, 0, 0, 1 )
.
% Evaluated N numbers, return longest
longest_chain_up_to_N( 0, 
                       _Current_Initial, 
                       _Current_Number, 
                       _Chain_Length, 
                       Longest_Length, 
                       Longest_Initial )
->
    {Longest_Initial, Longest_Length}
;
% Reached end of current chain, new longest chain found. 
longest_chain_up_to_N( N,
                       Current_Initial,
                       1,
                       Chain_Length,
                       Longest_Length,
                       _Longest_Initial ) when Chain_Length > Longest_Length
->
    longest_chain_up_to_N( N - 1,
                           Current_Initial + 1,
                           Current_Initial + 1,
                           1,
                           Chain_Length,
                           Current_Initial )
;
% Reached end of current chain
longest_chain_up_to_N( N,
                       Current_Initial,
                       1,
                       _Chain_Length,
                       Longest_Length,
                       Longest_Initial )
->
    longest_chain_up_to_N( N - 1,
                           Current_Initial + 1,
                           Current_Initial + 1,
                           1,
                           Longest_Length,
                           Longest_Initial )
;
% Go to next link in chain
longest_chain_up_to_N( N, 
                       Current_Initial, 
                       Current_Number,
                       Chain_Length, 
                       Longest_Length, 
                       Longest_Initial )
->
    case Current_Number rem 2 == 0 of
        true % Current_Number is even
        ->
            longest_chain_up_to_N( N,
                                   Current_Initial,
                                   Current_Number div 2,
                                   Chain_Length + 1,
                                   Longest_Length,
                                   Longest_Initial )
        ;
        false % Current_Number is odd
        ->
            longest_chain_up_to_N( N,
                                   Current_Initial,
                                   3*Current_Number + 1,
                                   Chain_Length + 1,
                                   Longest_Length,
                                   Longest_Initial )

    end
.
