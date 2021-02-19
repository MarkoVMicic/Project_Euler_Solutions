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
           build_list_of_primes/1] ).

main( N )
->
    Primes = build_list_of_primes( N ),
    find_circular_primes( Primes )
.

% ----------------------
% Sieve of Eratosthenes
% ----------------------
build_list_of_primes( N )
->
    build_list_of_primes( lists:seq(2, N), 2, [], [], math:sqrt(N) )
.
build_list_of_primes( [Current_Number | Unfiltered], 
                      Current_Prime, 
                      Filtered, 
                      Primes,
                      Root_N ) when Current_Number rem Current_Prime == 0
->
    build_list_of_primes( Unfiltered, 
                          Current_Prime,
                          Filtered,
                          Primes,
                          Root_N )
;
build_list_of_primes( [Current_Number | Unfiltered], 
                      Current_Prime, 
                      Filtered,
                      Primes, 
                      Root_N )
->
    build_list_of_primes( Unfiltered, 
                          Current_Prime,
                          [Current_Number | Filtered],
                          Primes,
                          Root_N )
;
build_list_of_primes( [], 
                      Current_Prime,
                      Filtered,
                      Primes,
                      Root_N ) when Current_Prime > Root_N
->
    lists:reverse(Primes) ++ lists:reverse( Filtered )
;
build_list_of_primes( [], 
                      Current_Prime,
                      Filtered,
                      Primes,
                      Root_N ) 
->
    New_Unfiltered = lists:reverse( Filtered ),
    [New_Prime | _] = New_Unfiltered,
    build_list_of_primes( New_Unfiltered,
                          New_Prime,
                          [],
                          [Current_Prime | Primes],
                          Root_N )
.


% -----------------------------------------------------------------------------
% Given a list of primes, this function extracts circular primes and puts them 
% in a list, and also counts them at the end. 
% NOTE: Constructing the list isn't necessary to count the number of circular 
%       primes, but since the list is small compared to the size of the Primes 
%       list, it's not a big deal. 
% -----------------------------------------------------------------------------
find_circular_primes( Primes )
->
    find_circular_primes( Primes, Primes, [] )
.
find_circular_primes( Primes, [Current_Prime | Rest], Circular_Primes )
->
    % TODO: This counts circular primes multiple times. Given the number of 
    %       circular primes is small and the amount of rotations that need to 
    %       be done is also small, optimizing this is not going to save too 
    %       much time, but it'll still help
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
% NOTE: All single-digit prime numbers are circular. Also since all 
%       multi-digit prime numbers with 2 and 5 are NOT circular, we need to 
%       make sure we return true for single-digit prime numbers FIRST. 
    true
;
is_circular_prime( Primes, Prime_String )
->
    % Check if 2,4,5,6,8,0 are in the Prime_String.    
    Disallowed_String_List = ["2", "4", "5", "6", "8", "0"],
    case prime_string_has_disallowed_digit(Prime_String, 
                                           Disallowed_String_List) of
        true
        ->
            % If any of 2,4,5,6,8,0 are in the prime number, it is not a 
            % circular prime.
            false
        ;
        false
        ->
            % If none of 2,4,5,6,8,0 are in the prime number, then it might be a circular prime. Convert the string to a queue for easier 
            % rotation. 
            Prime_Queue = queue:from_list( Prime_String ),  
            % NOTE: Final argument in this function is the number of rotations
            %       required. 
            is_circular_prime( Primes, Prime_Queue, length( Prime_String ) )
    end
.
is_circular_prime( _Primes, _Prime_Queue, 0 )
->
    true
;
is_circular_prime( Primes, Prime_Queue, Rotations_Remaining )
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
            is_circular_prime( Primes, Rotated_Queue, Rotations_Remaining-1 )
    end
.


prime_string_has_disallowed_digit( Prime_String, [Disallowed_Digit | Rest] )
->
    case lists:member([Disallowed_Digit], Prime_String ) of
        true  ->  true;
        false ->  prime_string_has_disallowed_digit( Prime_String, Rest )
    end 
;
prime_string_has_disallowed_digit( _Prime_String, [] )
->
    false
.


% -----------------------------------------------------------------------
% Queue Rotation. 
% Takes whatever is in the front of the queue and inserts it at the back
% -----------------------------------------------------------------------
rotate_queue( Queue )
->
    queue:in( queue:get(Queue), queue:drop(Queue) )
.
