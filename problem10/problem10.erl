% Problem link: https://projecteuler.net/problem=10


% Problem Statement: 

% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

% Find the sum of all the primes below two million.

-module(problem10).
-export([main/1]).

main( N )
->
    Primes_List = build_list_of_primes( N ),
    lists:sum(Primes_List)
.

% Sieve of Eratosthenes
build_list_of_primes( N ) 
->
    build_list_of_primes( [2], lists:seq(2, N), [] )
.
build_list_of_primes( Primes, [], [] ) 
-> 
    % Uncomment this line for primes in ascending order. 
    % lists:reverse( Primes )

    % Uncomment this line for primes in descending order.
    Primes
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
