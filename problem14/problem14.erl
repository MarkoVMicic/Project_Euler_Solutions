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
-export( [naive_main/1, fast_main/1] ).

naive_main( N )
->
    longest_chain_up_to_N( N )
.

fast_main( N )
->
    longest_chain_with_map( N )
.

% ---------------
% Naive solution
% ---------------
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

% --------------
% Fast solution
% --------------
longest_chain_with_map( N ) 
-> 
    % Use a map to store previously seen chain-lengths. See (1) in bottom 
    % comment for details
    % Begin searching from N div 2. See (2) in bottom comment for details. 
    longest_chain_with_map( N div 2, 
                            N div 2, 
                            N div 2, 
                            0, 
                            0, 
                            1, 
                            #{} )
.
longest_chain_with_map( 0, 
                       _Current_Initial, 
                       _Current_Number, 
                       _Chain_Length, 
                       Longest_Length, 
                       Longest_Initial,
                       _Map )
->
    {Longest_Initial, Longest_Length+1}
;
longest_chain_with_map( N,
                        Current_Initial,
                        1,
                        Chain_Length,
                        Longest_Length,
                        _Longest_Initial,
                        Map ) when Chain_Length > Longest_Length
->
    longest_chain_with_map( N-1,
                            Current_Initial + 1,
                            Current_Initial + 1,
                            0,
                            Chain_Length,
                            Current_Initial,
                            Map#{Current_Initial => Chain_Length} )
;
longest_chain_with_map( N,
                        Current_Initial,
                        1,
                        Chain_Length,
                        Longest_Length,
                        Longest_Initial,
                        Map ) 
->
    longest_chain_with_map( N-1, 
                            Current_Initial + 1,
                            Current_Initial + 1,
                            0,
                            Longest_Length,
                            Longest_Initial,
                            Map#{Current_Initial => Chain_Length} )
;
% Current Number is even: if we've seen Current_Number/2 before, we can 
% immediately compute the length (See (2))
longest_chain_with_map( N,
                        Current_Initial,
                        Current_Number,
                        Chain_Length, 
                        Longest_Length,
                        Longest_Initial,
                        Map ) when Current_Number rem 2 == 0
->
    case Map of
        #{(Current_Number div 2) := Stored_Length}
        ->
            longest_chain_with_map( N,
                                    Current_Initial,
                                    1,
                                    Chain_Length + (Stored_Length + 1),
                                    Longest_Length,
                                    Longest_Initial,
                                    Map )
        ;
        _
        ->
            longest_chain_with_map( N,
                                    Current_Initial,
                                    Current_Number div 2,
                                    Chain_Length + 1,
                                    Longest_Length,
                                    Longest_Initial,
                                    Map )
    end
;
% Current Number is odd: if we've seen (3*Current_Number+1)/2 before, we can 
% immediately compute the length (See (3))
longest_chain_with_map( N,
                        Current_Initial,
                        Current_Number,
                        Chain_Length, 
                        Longest_Length,
                        Longest_Initial,
                        Map ) when Current_Number rem 2 == 1
->
    case Map of
        #{((3*Current_Number + 1) div 2) := Stored_Length}
        ->
            longest_chain_with_map( N,
                                    Current_Initial,
                                    1,
                                    Chain_Length + (Stored_Length + 2),
                                    Longest_Length,
                                    Longest_Initial,
                                    Map )
        ;
        _
        ->
            longest_chain_with_map( N,
                                    Current_Initial,
                                    3*Current_Number +1,
                                    Chain_Length + 1,
                                    Longest_Length,
                                    Longest_Initial,
                                    Map )
    end
.


% (1) We can store the values seen thus far, and if we find
%     a previously seen value, we can simply add 1 to that,
%     e.g. since Collatz(13) = 10, and for initial 26, the
%     chain begins 26 -> 13, clearly
%     Collatz(26) = 1 + Collatz(13).
% (2) If K is even, then Collatz(K) = 1 + Collatz(K/2). Thus,
%     for all K, Collatz(2K) > Collatz(K). Thus for input N,
%     we need only to check the values M ≥ N/2.
% (3) If N is odd, then 3N+1 is even, so using (2), we see
%     that Collatz(N) = Collatz((3N + 1)/2) + 2.
