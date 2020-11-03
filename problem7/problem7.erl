% Problem link: https://projecteuler.net/problem=7


% Problem statement:

% By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

% What is the 10 001st prime number?

-module(problem7).
-export([main/1]).

main( N )
->
    % Using the prime number theorem, we know that if π(N) is the number of 
    % primes less than or equal to N, then 
    %   π(N) ~ N/log(N)
    % using a magic number 1.3 as a scaling factor (which I arrived at via 
    % experimentation, I'm able to ensure that for all N > 2, the length of 
    % the list of generated primes length(Primes) > N. After that, I just 
    % simply trim off the values until I have a list of values of length N, 
    % and since the list is in descending order, it's easy enough to just trim 
    % the head off until we have a length of N. Indeed, we even can store the 
    % difference between length(Primes) and N, and just store that difference 
    % instead of recomputing the length every time. 
    Primes = build_list_of_primes( trunc(math:ceil(1.3*N*math:log(N))) ),
    Difference = length(Primes) - N,
    [Nth_prime | _Rest] = trim_list_of_primes(Primes, Difference),
    Nth_prime
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

trim_list_of_primes( Primes, 0 )          -> Primes;
trim_list_of_primes( [_Prime | Rest], D ) -> trim_list_of_primes( Rest, D-1 ).
