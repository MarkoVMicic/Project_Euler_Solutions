% Problem link: https://projecteuler.net/problem=5


% Problem statement:
    
% 2520 is the smallest number that can be divided by each of the numbers from 
% 1 to 10 without any remainder.

% What is the smallest positive number that is evenly divisible by all of the 
% numbers from 1 to 20?

-module(problem5).
-export([naive_main/1, fast_main/1]).

% Naive solution: simply brute force through all numbers until you find a 
% number that is evenly divisible by all integers [1, 2, ..., N].
naive_main( N )
->
    naive_find_number_evenly_divisible_up_to_n( 1, 1, N )
.

% See huge comment at the bottom of file for math explanation of how this fast 
% version works. 
fast_main( N )
->
    Primes = build_list_of_primes( N ),
    Limit = math:sqrt( N ),
    LogN = math:log( N ),
    fast_find_number_evenly_divisible_up_to_n( Primes, Limit, LogN, 1 )
.

% Naive solution: Iterate through all divisors until N, and increment your 
% number by 1 everytime you find something that isn't evenly divisible. 
naive_find_number_evenly_divisible_up_to_n( N, Number, N ) -> Number;
naive_find_number_evenly_divisible_up_to_n( Factor, 
                                            Number, 
                                            N ) when Number rem Factor == 0
->
    naive_find_number_evenly_divisible_up_to_n( Factor + 1, 
                                                Number, 
                                                N )
;
naive_find_number_evenly_divisible_up_to_n( _Factor, Number, N )
->
    naive_find_number_evenly_divisible_up_to_n( 1, Number+1, N )
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fast solution: 


% First, build a list of all primes up to N.
% This is just the Sieve of Eratosthenes. 
% You have 3 lists: 
% • Primes: a list of the found prime numbers
% • Numbers: a list of the current integers
% • Filtered: a list of the integers that have gotten through the sieve. 
% Starting from Primes = [2], check if the head of Numbers is divisible by the 
% head of Primes (Current_Prime). If it is, simply remove it from the list. If it isn't, remove it from Numbers while also putting it in the Filtered list. 
% Then when Numbers is empty, reverse the Filtered List, remove the first 
% element and put it as the new head of the Primes list. Continue until you've 
% built up all the primes in a list. 
% Simple worked example: primes up to 10.

% Primes      [2]                     ->  [2]
% Number      [2,3,4,5,6,7,8,9,10]    ->  [3,4,5,6,7,8,9,10]
% Filtered    []                      ->  []

% Primes      [2]               ->  [2]             ->  [2]
% Number      [4,5,6,7,8,9,10]  ->  [5,6,7,8,9,10]  ->  [6,7,8,9,10]
% Filtered    [3]               ->  [3]             ->  [5,3]

% Primes      [2]         ->  [2]       ->  [2]     ->  [2]
% Number      [7,8,9,10]  ->  [8,9,10]  ->  [9,10]  ->  [10]
% Filtered    [5,3]       ->  [7,5,3]   ->  [7,5,3] ->  [9,7,5,3]

% Primes      [2]       ->  [3,2]    ->  [3,2]  ->  [3,2]   ->  [3,2]
% Number      []        ->  [5,7,9]  ->  [7,9]  ->  [9]     ->  []
% Filtered    [9,7,5,3] ->  []       ->  [5]    ->  [7,5]   ->  [7,5]

% Primes      [5,3,2]   ->  [5,3,2] ->  [7,5,3,2]
% Number      [7]       ->  []      ->  []
% Filtered    []        ->  [7]     ->  []
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

% Then for each prime in the list of primes, compute the appropriate exponent 
% as follows:
% Let Number = 1
% Let Limit = sqrt(N)
% Let LogN = log(N). 
% If Prime <= Limit, then Number = Number * exp(Prime, floor(LogN/log(Prime)))
% If Prime > Limit, then Number = Number * Prime. 
fast_find_number_evenly_divisible_up_to_n( [], _, _, Number ) -> Number;
fast_find_number_evenly_divisible_up_to_n( [Current_Prime | Rest],
                                           Limit,
                                           LogN,
                                           Number ) when Current_Prime =< Limit
->
    % Trunc is used as an integer floor function. 
    Exponent = trunc( LogN / math:log(Current_Prime) ),
    New_Number = Number * exp( Current_Prime, Exponent ),
    fast_find_number_evenly_divisible_up_to_n( Rest,
                                               Limit,
                                               LogN,
                                               New_Number )
;
fast_find_number_evenly_divisible_up_to_n( [Current_Prime | Rest],
                                           Limit,
                                           LogN,
                                           Number )
->
    fast_find_number_evenly_divisible_up_to_n( Rest,
                                               Limit,
                                               LogN,
                                               Number*Current_Prime )
.

% Square-multiply algorithm for fast exponentiation. As an added benefit, it 
% also keeps things as integers which is what we need in this program. 
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

% Fast solution: Suppose that Number is the variable representing the smallest 
% number that is evenly ivisible by all integers [1, 2, ..., N]. Since all 
% numbers are evenly divisible by 1, let's consider the list [2, ..., N].
% Then by the fundamental theorem of arithmetic, Number must also be evenly 
% divisible by all Prime numbers <= N, since each number in [2, ..., N] has a 
% prime factorization. It is sufficient to consider only the prime numbers up 
% to N, while paying attention to their exponents. 
% e.g. Look at the first 3 values of N:
%     N=2, Number = 2
%     N=3, Number = 2*3 = 6
%     N=4, Number = 2*3*2 = 12
% Notice how  we did not need to evaluate 2*3*4, because 2*2 is the prime 
% factorization of 4. 
% Building on top of this:
%     N=5, Number = 2*3*2*5 = 60
%     N=6, Number = 2*3*2*5 = 60
% Note that the above statement for N=6 shows us that by simply considering 
% all possibilities of Number such that 
%   Number rem 2 == 0 andalso Number rem 3 == 0 
% we automatically get all the possibilities of Number such that 
%   Number rem 6 == 0. 
% Thus it is sufficient to consider only the prime numbers up to N, and we 
% then need to consider their respective exponents. 
% So now suppose that we have some array of Prime numbers, 
% PrimeArr = [2, 3, ..., P_k] where P_k <= N is the largest prime less than or 
% equal to N. 
% Then to determine the exponent needed for each of these prime numbers, we 
% can first note that exp(P_i, J) must be less than N in order for it to 
% capture information about all the possible factors in the list [2, ..., N]. 
% For example, for N=10, we note that 2^3 = 8 < 10, but 2^4 = 16 > 10, so that 
% the exponent for 2 is 3. Similarly, 3^2 = 9 but 3^3 = 27, so the exponent 
% for 3 is 2. For the rest of the primes up to 10, the exponent is 1, so that 
% we have exp(2,3)*exp(3,2)*5*7 = 2*2*2*3*3*5*7 = 2520 as expected. 
% To compute this exponent, we simply observe
%     exp(P_i, J) <= N
%     log(exp(P_i, J)) <= log(N)
%     J*log(P_i) <= log(N)
%     J <= log(N)/log(P_i)
% and since J must be an integer, we can simply take the floor:
%     J = floor(log(N)/log(P_i))
% Note that after a certain prime number, the exponents are all 1. This 
% happens when exp(P_i,2) > N. Thus we know that for all P_i > sqrt(N), the 
% exponent is 1. 
