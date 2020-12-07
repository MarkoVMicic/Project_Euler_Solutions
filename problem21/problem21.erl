% Problem link: https://projecteuler.net/problem=21


% Problem Statement:

% Let d(n) be defined as the sum of proper divisors of n (numbers less than n 
% which divide evenly into n).
% If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and 
% each of a and b are called amicable numbers.

% For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 
% 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 
% 71 and 142; so d(284) = 220.

% Evaluate the sum of all the amicable numbers under 10000.

-module( problem21 ).
-export( [naive_main/1, 
          fast_main/1, 
          fast_prime_main/1,
          fast_read_file_prime_main/2,
          build_list_of_primes_and_write_to_file/2] ).

naive_main( N )
->
    sum_amicable_numbers( N )
.

fast_main( N )
->
    sum_amicable_numbers_fast( N )
.

fast_prime_main( N )
->
    sum_amicable_numbers_fast_prime( N )
.

fast_read_file_prime_main( N, Input_Path_Primes )
->
    case file:read_file( Input_Path_Primes ) of
        {ok, Binary}
        ->
            Primes = binary_to_term( Binary ),
            sum_amicable_numbers_fast_prime_list( N, Primes )
        ;
        {error, Reason}
        ->
            {error, Reason}
    end
.

% We begin from 2 because 1 has no proper divisors according to the definition 
% of the problem statement. 
sum_amicable_numbers( N ) -> sum_amicable_numbers( N, 2, 0 ).
sum_amicable_numbers( N, Current_Number, Acc) when Current_Number == N
->
    Acc
;
sum_amicable_numbers( N, Current_Number, Acc )
->
    Sum_Divisors = sum_divisors( Current_Number ),
    % Note that according to the problem, a number cannot be amicable with 
    % itself, so we check for that equality here.
    case sum_divisors( Sum_Divisors ) == Current_Number andalso 
         Sum_Divisors =/= Current_Number                of
        true    
        ->  
            io:fwrite("~p, ~p are amicable numbers.~n", [Current_Number, Sum_Divisors]),
            sum_amicable_numbers( N, Current_Number+1, Acc+Current_Number );
        false
        ->
            sum_amicable_numbers( N, Current_Number+1, Acc )
    end
.

sum_divisors( N ) -> sum_divisors( N, 1, 0 ).
sum_divisors( N, Current_Number, Acc ) when Current_Number == N
->
    Acc
;
sum_divisors( N, Current_Number, Acc ) when N rem Current_Number == 0
->
    sum_divisors( N, Current_Number+1, Acc+Current_Number )
;
sum_divisors( N, Current_Number, Acc )
->
    sum_divisors( N, Current_Number+1, Acc )
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Faster version of summing amicable 
% numbers up to N. This cuts the number of
% times we evaluate sum_divisors in half 
% by filtering for when 
% sum_divisors( N ) > N. Makes the code 
% uglier. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sum_amicable_numbers_fast( N ) -> sum_amicable_numbers_fast( N, 2, 0 ).
sum_amicable_numbers_fast( N, Current_Number, Acc ) when Current_Number == N
->
    Acc
;
sum_amicable_numbers_fast(N, Current_Number, Acc )
->
    Sum_Divisors = sum_divisors_fast( Current_Number ),
    if 
        Sum_Divisors > Current_Number 
        ->
            case sum_divisors_fast( Sum_Divisors ) == Current_Number andalso 
                 Sum_Divisors =/= Current_Number                of
            true    
            ->  
                io:fwrite("~p, ~p are amicable numbers.~n", [Current_Number, Sum_Divisors]),
                sum_amicable_numbers_fast( N, 
                                           Current_Number+1, 
                                           Acc+Current_Number+Sum_Divisors );
            false
            ->
                sum_amicable_numbers_fast( N, 
                                           Current_Number+1, 
                                           Acc )
            end
        ;
        true
        ->
            sum_amicable_numbers_fast( N, 
                                       Current_Number+1,
                                       Acc )
    end
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Faster version of summing the divisors 
% of N. Uses sqrt(N) as an upper bound for
% divisors, and leverages the fact that 
% odd numbers only have odd divisors. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sum_divisors_fast( 2 ) -> 1;
sum_divisors_fast( 3 ) -> 1;
sum_divisors_fast( N ) 
-> 
    Sqrt_N = trunc( math:sqrt(N) ),
    case Sqrt_N * Sqrt_N == N of
        % If n is a perfect square, then we need to add its square root to the 
        % sum of proper divisors since it will be otherwise missed by the algorithm 
        true
        ->
            sum_divisors_fast( N, Sqrt_N, 1, Sqrt_N)
        ;
        false
        ->
            sum_divisors_fast( N, Sqrt_N, 1, 0)            
    end
.
sum_divisors_fast( N, 
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when Current_Number >= Sqrt_N
->
    Acc - N
;
% If N is odd, it cannot have even divisors, so we can increment 
% Current_Number by 2. 
sum_divisors_fast( N,
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when N rem 2 == 1,
                              N rem Current_Number == 0
->
    sum_divisors_fast( N,
                       Sqrt_N, 
                       Current_Number + 2,
                       Acc + Current_Number + (N div Current_Number) )
;
sum_divisors_fast( N,
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when N rem 2 == 1
->
    sum_divisors_fast( N,
                       Sqrt_N, 
                       Current_Number + 2,
                       Acc )
;
% If N is even, then it can have both even and odd divisors, so we can only 
% increment Current_Number by 1. 
sum_divisors_fast( N,
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when N rem 2 == 0,
                              N rem Current_Number == 0
->
    sum_divisors_fast( N,
                       Sqrt_N, 
                       Current_Number + 1,
                       Acc + Current_Number + (N div Current_Number) )
;
sum_divisors_fast( N,
                   Sqrt_N, 
                   Current_Number, 
                   Acc ) when N rem 2 == 0
->
    sum_divisors_fast( N,
                       Sqrt_N, 
                       Current_Number + 1,
                       Acc )
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This uses a precomputed list of prime 
% numbers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sum_amicable_numbers_fast_prime_list( N, Primes )
->
    sum_amicable_numbers_fast_prime( N, Primes, 2, 0)
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This generates a list of prime numbers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sum_amicable_numbers_fast_prime( N )
->
    Primes = build_list_of_primes( N div 2),
    sum_amicable_numbers_fast_prime( N, Primes, 2, 0 )
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Use list of prime numbers to compute the
% sum of amicable numbers up to N. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sum_amicable_numbers_fast_prime( N, 
                                 _Primes, 
                                 Current_Number, 
                                 Acc ) when Current_Number == N
->
    Acc
;
sum_amicable_numbers_fast_prime( N,
                                 Primes,
                                 Current_Number,
                                 Acc )
->
    Sum_Divisors = sum_divisors_fast_prime( Current_Number, Primes ),
    if 
        Sum_Divisors > Current_Number 
        ->
            case sum_divisors_fast_prime( Sum_Divisors, Primes ) == Current_Number andalso 
                 Sum_Divisors =/= Current_Number                                   of
            true    
            ->  
                io:fwrite("~p, ~p are amicable numbers.~n", 
                          [Current_Number, Sum_Divisors]),
                sum_amicable_numbers_fast_prime( N,
                                                 Primes, 
                                                 Current_Number+1, 
                                                 Acc+Current_Number+Sum_Divisors );
            false
            ->
                sum_amicable_numbers_fast_prime( N,
                                                 Primes, 
                                                 Current_Number+1, 
                                                 Acc )
            end
        ;
        true
        ->
            sum_amicable_numbers_fast_prime( N,
                                             Primes, 
                                             Current_Number+1,
                                             Acc )
    end
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Use a list of prime numbers to compute 
% the divisors of a number.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sum_divisors_fast_prime( 2, _Primes ) -> 1;
sum_divisors_fast_prime( 3, _Primes ) -> 1;
sum_divisors_fast_prime( N, Primes ) 
->
    case lists:member( N, Primes ) of
      true  -> 1;
      false -> sum_divisors_fast_prime( N, N, Primes, 1 )
    end
.
sum_divisors_fast_prime( N, 
                         Current_N,
                         [Prime | _Primes], 
                         Acc ) when Prime*Prime > Current_N andalso
                                    Current_N == 1
->
    Acc - N
;
sum_divisors_fast_prime( N, 
                         Current_N,
                         [Prime | _Primes], 
                         Acc ) when Prime*Prime > Current_N
->
    Acc*(Current_N+1) - N
;

sum_divisors_fast_prime( N,
                         Current_N, 
                         [Prime | Primes], 
                         Acc ) when Current_N rem Prime == 0
->
    Exponent = find_exponent( Current_N, Prime ),
    Factor = (exp( Prime, Exponent+1 ) - 1) div ( Prime - 1 ),
    sum_divisors_fast_prime( N,
                             Current_N div exp(Prime, Exponent),
                             Primes,
                             Acc*Factor )
;
sum_divisors_fast_prime( N,
                         Current_N,
                         [_Prime | Primes],
                         Acc )
->
    sum_divisors_fast_prime( N,
                             Current_N,
                             Primes,
                             Acc )
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sieve of Eratosthenes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_list_of_primes( N ) 
->
    build_list_of_primes( [2], lists:seq(2, N), [] )
.
build_list_of_primes( Primes, [], [] ) 
-> 
    % Uncomment this line for primes in ascending order. 
    lists:reverse( Primes )

    % Uncomment this line for primes in descending order.
    % Primes
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Build list using Sieve and write to file. 
% Useful as a precomputation step. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_list_of_primes_and_write_to_file( N, Output_Path )
->
    Primes = build_list_of_primes( N ),
    Binary = term_to_binary( Primes ),
    case file:write_file( Output_Path, Binary ) of
        ok              -> ok;
        {error, Reason} -> {error, Reason}
    end
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% If N = P^X, this determines X
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_exponent( N, P ) -> find_exponent( N, P, 0 ).
find_exponent( N, P, X ) when N rem P =/= 0
->
    X
;
find_exponent( N, P, X )
->
    find_exponent( N div P,
                   P,
                   X+1 )
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Square-multiply algorithm for fast 
% exponentiation. As an added benefit, it 
% also keeps things as integers which is
% what we need in this program. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
exp( B, E ) when E > 3  -> lexp( B, split_exp(E, []), B );
exp( B, 3 )             -> B*B*B;
exp( B, 2 )             -> B*B;
exp( B, 1 )             -> B;
exp( _, 0 )             -> 1.

lexp( B, [sqr|L], Acc ) -> lexp( B, L, Acc*Acc );
lexp( B, [_|L], Acc )   -> lexp( B, L, Acc*B );
lexp( _, _, Acc )       -> Acc.

split_exp( E, L ) when E < 2  -> L;
split_exp( E, L ) 
->
    case ( E rem 2 == 0 ) of
         true   -> split_exp( E div 2, [sqr|L] );
         false  -> split_exp( E - 1, [multiply|L] )
    end
.
