% Problem link: https://projecteuler.net/problem=4


% Problem statement:
    
% A palindromic number reads the same both ways. The largest palindrome made 
% from the product of two 2-digit numbers is 9009 = 91 × 99.

% Find the largest palindrome made from the product of two 3-digit numbers.

-module(problem4).
-export([main/1]).

% TODO: Needs a check to ensure that the Current_factor never has fewer digits 
% than Num_digits. Can do this by keeping track of a Minimum_factor. 

main( Num_digits ) 
->
    % First, find the largest number that can be formed by multiplying two 
    % integers each with number of digits == Num_digits. For the problem 
    % statement, Num_digits = 3, so Max_number is 999*999 == 998001
    Max_number = find_max_number_from_num_digits( Num_digits ),
    % Starting from Max Number and going downwards, find the largest 
    % palindrome that is the product of 2 integers, while making sure that the 
    % integers never have too many (or too few) digits. 
    [Factor1, Factor2] = find_largest_palindrome( Max_number, 
                                                  Num_digits ),
    [Factor1*Factor2, Factor1, Factor2]
.

find_max_number_from_num_digits( Num_digits ) 
->
    Max_factor = find_max_factor( Num_digits ),
    Max_number = Max_factor * Max_factor,
    Max_number
.

find_max_factor( Num_digits ) -> find_max_factor( 9, 1, Num_digits ).

find_max_factor( Current_number, 
                 Current_digits, 
                 Required_Digits ) when Current_digits =:= Required_Digits 
->
    Current_number
;
find_max_factor( Current_number, 
                 Current_digits, 
                 Required_Digits ) 
->
    Current_number_list = digitize_number( Current_number ),
    New_number_list = [9 | Current_number_list],
    New_number = undigitize_number( New_number_list ),
    find_max_factor( New_number, 
                     Current_digits + 1, 
                     Required_Digits )
.

find_largest_palindrome( Number, Num_digits ) 
->
    % Finds the largest palindrome =< number that is the product of 2 integers.
    Max_factor = find_max_factor( Num_digits ),
    find_largest_palindrome( Max_factor, 
                             Number, 
                             Max_factor )
.
find_largest_palindrome( Current_factor, 
                         Number, 
                         Max_factor ) 
->    
    % • If Number is not a palindrome, decrease by one. 
    % • If Number is a Palindrome, Then start keeping track of a quotient, 
    %   call find_largest_palindrome/4
    case ( is_palindrome(Number) ) of
        true 
        -> 
            find_largest_palindrome( Current_factor, 
                                     Number, 
                                     Max_factor, 
                                     Number div Current_factor )
        ;
        false 
        -> 
            find_largest_palindrome( Current_factor,
                                     Number - 1,
                                     Max_factor )
    end
.
% If the Palindrome is evenly divisible by the Current_factor and the quotient 
% is not too large, then return the pair of factors which now comprise the two 
% factors of the largest palindrome
find_largest_palindrome( Current_factor, 
                         Palindrome, 
                         Max_factor, 
                         _ ) when Palindrome rem Current_factor == 0,
                                  Palindrome div Current_factor < Max_factor 
->
    [Current_factor, Palindrome div Current_factor]
;
% If the quotient is larger than the max factor, decrease Palindrome by 1 (so 
% that it is no longer a palindrome) and find the next palindrome by calling 
% find_largest_palindrome/3
find_largest_palindrome( Current_factor, 
                         Palindrome, 
                         Max_factor, 
                         _ ) when Palindrome div Current_factor > Max_factor 
->
    find_largest_palindrome( Max_factor, 
                             Palindrome-1, 
                             Max_factor )
;
% If the Palindrome is not evenly divisible by the Current_factor, decrease 
% the current factor by 1 (while also tracking the quotient of the integer 
% division of Palindrome by Current_factor
find_largest_palindrome(Current_factor, 
                        Palindrome, 
                        Max_factor, 
                        _ ) when Palindrome rem Current_factor =/= 0 
->
    find_largest_palindrome( Current_factor-1, 
                             Palindrome, 
                             Max_factor, 
                             Palindrome div Current_factor )
.


is_palindrome( Number ) 
->
    digitize_number( Number ) == lists:reverse( digitize_number(Number) )
.
% Converts a base-10 number N into a list of its digits in base 10. 
% The head of the list is the first digit i.e. 12345 -> [1,2,3,4,5]
digitize_number( N ) when N < 10  -> [N];
digitize_number( N ) when N >= 10 -> digitize_number( N div 10 ) ++ [N rem 10].

% Converts a list of digits into a base-10 number. 
undigitize_number( Number_list ) -> undigitize_number( Number_list, 0 ).
undigitize_number( [], Number )  -> Number;
undigitize_number( [Current_digit | Rest], Number ) 
->
    undigitize_number( Rest, Number + Current_digit*exp(10,length(Rest)) )
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
