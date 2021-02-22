-module( primes_lib ).
-export( [ build_list_of_primes/1 ] ).



% ----------------------------------
% Sieve of Eratosthenes
% Builds list of primes from 2 to N
% ---------------------------------
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