% Problem link: https://projecteuler.net/problem=35


% Problem Statement:
% The number, 197, is called a circular prime because all rotations of the 
% digits: 197, 971, and 719, are themselves prime.

% There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 
% 71, 73, 79, and 97.

% How many circular primes are there below one million?   

-module( problem35 ).
-export( [ main/1,
           is_circular_prime/2,
           build_list_of_primes/1 ] ).

main( N )
->
    Primes = build_list_of_primes( N ),
    find_circular_primes( Primes )
.

% TODO: Find a way to speed this up by only checking up to √N, after which all 
%       remaining unfiltered numbers should be prime.
% ----------------------
% Sieve of Eratosthenes
% ----------------------
build_list_of_primes( N ) 
->
    build_list_of_primes( [2], lists:seq(2, N), [] )
.
build_list_of_primes( Primes, [], [] ) 
-> 
    lists:reverse( Primes )
; 
build_list_of_primes( Primes, [], Numbers )
->
    [Next_Prime | Rest] = lists:reverse( Numbers ),
    build_list_of_primes( [Next_Prime | Primes], Rest, [] )
;
build_list_of_primes( [Current_Prime  | Primes], 
                      [Current_Number | Numbers], 
                      Filtered ) when Current_Number rem Current_Prime == 0
->
    build_list_of_primes( [Current_Prime | Primes],
                          Numbers,
                          Filtered )
;
build_list_of_primes( [Current_Prime  | Primes], 
                      [Current_Number | Numbers], 
                      Filtered )
->
    build_list_of_primes( [Current_Prime  | Primes],
                          Numbers,
                          [Current_Number | Filtered] )
.

% TODO: Speed up by checking for even numbered digits -- instant reject
% TODO: Speed up by checking for presence of 5 -- instant reject
find_circular_primes( Primes )
->
    find_circular_primes( Primes, Primes, [] )
.
find_circular_primes( Primes, [Current_Prime | Rest], Circular_Primes )
->
    % TODO: This counts circular primes multiple times.
    case is_circular_prime( Primes, integer_to_list(Current_Prime) ) of
        true
        ->
            find_circular_primes( Primes, Rest, [Current_Prime | Circular_Primes] )
        ;
        false
        ->
            find_circular_primes( Primes, Rest, Circular_Primes )        
    end
;
find_circular_primes( _Primes, [], Circular_Primes )
->
    {length( Circular_Primes ), Circular_Primes}
.


is_circular_prime( _Primes, Prime_String ) when length(Prime_String) == 1
->
    true
;
is_circular_prime( Primes, Prime_String )
->
    Prime_Queue = queue:from_list( Prime_String ),  
    is_circular_prime( Primes, Prime_Queue, length( Prime_String ) )
.
is_circular_prime( _Primes, _Prime_Queue, 0 )
->
    true
;
is_circular_prime( Primes, Prime_Queue, N )
->
    Rotated_Queue = rotate_queue( Prime_Queue ),
    Rotated_String = queue:to_list( Rotated_Queue ),
    Rotated_Prime = list_to_integer( Rotated_String ),
    case lists:member( Rotated_Prime, Primes ) of
        false
        ->
            false
        ;
        true
        ->
            is_circular_prime( Primes, Rotated_Queue, N-1 )
    end
.


rotate_queue( Queue )
->
    queue:in( queue:get(Queue), queue:drop(Queue) )
.

