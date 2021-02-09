% Problem link: https://projecteuler.net/problem=27


% Problem Statement:
% Euler discovered the remarkable quadratic formula:

%     N^2 + N + 41

% It turns out that the formula will produce 40 primes for the consecutive 
% integer values 0 ≤ N ≤ 39. However, when N = 40, 

%     40^2 + 40 + 41 = 40(40+1) + 41 

% is divisible by 41, and certainly when N = 41, is clearly divisible by 41.

% The incredible formula N^2 - 79N + 1601 was discovered, which produces 80 
% primes for the consecutive values 0 ≤ N ≤ 79. The product of the 
% coefficients, −79 and 1601, is −126479.

% Considering quadratics of the form:
    
%     N^2 + AN + B,

% where  
    
%     |A| < 1000 and |B| ≤ 1000

% Find the product of the coefficients, A and B, for the quadratic expression 
% that produces the maximum number of primes for consecutive values of N, 
% starting with N = 0.

-module( problem27 ).
-export( [ main/3 ] ).

main( Max_N, Abs_Max_A, Abs_Max_B )
->
    % Given in the problem
    Max_Prime_Number = find_max_prime( 79, -79, 1601 ),
    Primes = build_list_of_primes( Max_Prime_Number ),
    find_coefficient_product( Primes, Max_N, Abs_Max_A, Abs_Max_B )
.

find_max_prime( Max_N, A, B )
->
    find_max_prime( Max_N, A, B, 0, 2 )
.
find_max_prime( Max_N, _A, _B, N, Max_Prime ) when N > Max_N
->
    Max_Prime
;
find_max_prime( Max_N, A, B, N, Max_Prime ) when N*N + A*N + B > Max_Prime
->
    find_max_prime( Max_N, A, B, N + 1, N*N + A*N + B )
;
find_max_prime( Max_N, A, B, N, Max_Prime )
->
    find_max_prime( Max_N, A, B, N+1, Max_Prime )
.


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

find_coefficient_product( Primes, 
                          Max_N, 
                          Abs_Max_A, 
                          Abs_Max_B )
->
    find_coefficient_product( Primes,
                              Max_N,
                              Abs_Max_A,
                              Abs_Max_B,
                              0,
                              -Abs_Max_A,
                              -Abs_Max_B,
                              0,
                              {-Abs_Max_A, -Abs_Max_B, 0} )
.
% TODO: Parameter Length is unnecessary, it is already represented by N. 
% When A > Abs_Max_A, we are done.
find_coefficient_product( _Primes,
                          _Max_N,
                          Abs_Max_A, 
                          _Abs_Max_B,
                          _N,
                          A,
                          _B,
                          _Length,
                          {Max_A, Max_B, Max_Length} ) when A > Abs_Max_A
->
    {Max_A*Max_B, Max_Length}
;
% When N > Max_N, we are done. (This shouldn't happen!)
find_coefficient_product( _Primes,
                          Max_N,
                          _Abs_Max_A, 
                          _Abs_Max_B,
                          N,
                          _A,
                          _B,
                          _Length,
                          {Max_A, Max_B, Max_Length} ) when N > Max_N
->
    io:fwrite("Oops~n"),
    {Max_A*Max_B, Max_Length}
;
% if A + B < 2, then for N = 0, we clearly don't start with a prime. 
find_coefficient_product( Primes,
                          Max_N,
                          Abs_Max_A, 
                          Abs_Max_B,
                          _N,
                          A,
                          B,
                          _Length,
                          {Max_A, Max_B, Max_Length} ) when A + B < 2
->
    find_coefficient_product( Primes,
                              Max_N,
                              Abs_Max_A, 
                              Abs_Max_B,
                              0,
                              A,
                              B + 1,
                              0,
                              {Max_A, Max_B, Max_Length} )
;
% When B exceeds Abs_Max_B, we iterate A up by 1 and start B from -Abs_Max_B again. 
find_coefficient_product( Primes,
                          Max_N,
                          Abs_Max_A, 
                          Abs_Max_B,
                          N,
                          A,
                          B,
                          Length,
                          {Max_A, Max_B, Max_Length} ) when B > Abs_Max_B
->
    find_coefficient_product( Primes,
                              Max_N,
                              Abs_Max_A, 
                              Abs_Max_B,
                              N,
                              A+1,
                              -Abs_Max_B,
                              Length,
                              {Max_A, Max_B, Max_Length} )
;
% If the current length of the prime-number sequence is greater than 
% max_length, replace the values in the tuple, and iterate depending on if the 
% quadratic evaluates to a prime number or not. 
find_coefficient_product( Primes,
                          Max_N,
                          Abs_Max_A, 
                          Abs_Max_B,
                          N,
                          A,
                          B,
                          Length,
                          {_Max_A, _Max_B, Max_Length} ) when 
                                                         Length > Max_Length
->
    Value = eval_quadratic( N, A, B ),
    % If Value is a prime number, iterate N and add 1 to length.
    % Else reset N to 0, iterate B
    case lists:member( Value, Primes ) of
        true
        ->
            find_coefficient_product( Primes,
                                      Max_N,
                                      Abs_Max_A, 
                                      Abs_Max_B,
                                      N+1,
                                      A,
                                      B,
                                      Length+1,
                                      {A, B, Length+1} )
        ;
        false
        ->
            find_coefficient_product( Primes,
                                      Max_N,
                                      Abs_Max_A,
                                      Abs_Max_B,
                                      0,
                                      A,
                                      B + 1,
                                      1,
                                      {A, B, Length} )
    end
;
% If the current length of the prime-number sequence is less than 
% max_length, keep the values in the tuple, and iterate depending on if the 
% quadratic evaluates to a prime number or not. 
find_coefficient_product( Primes,
                          Max_N,
                          Abs_Max_A, 
                          Abs_Max_B,
                          N,
                          A,
                          B,
                          Length,
                          {Max_A, Max_B, Max_Length}) when Length =< Max_Length
->
    Value = eval_quadratic( N, A, B ),
    % If Value is a prime number, iterate N and add 1 to length.
    % Else reset N to 0, iterate B
    case lists:member( Value, Primes ) of
        true
        ->
            find_coefficient_product( Primes,
                                      Max_N,
                                      Abs_Max_A, 
                                      Abs_Max_B,
                                      N+1,
                                      A,
                                      B,
                                      Length+1,
                                      {Max_A, Max_B, Max_Length})
        ;
        false
        ->
            find_coefficient_product( Primes,
                                      Max_N,
                                      Abs_Max_A,
                                      Abs_Max_B,
                                      0,
                                      A,
                                      B + 1,
                                      1,
                                      {Max_A, Max_B, Max_Length} )
    end
.

eval_quadratic( N, A, B ) -> N*N + A*N + B.