% Problem link: https://projecteuler.net/problem=36


% Problem Statement:
% The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.

% Find the sum of all numbers, less than one million, which are palindromic in 
% base 10 and base 2.

% (Please note that the palindromic number, in either base, may not include 
% leading zeros.)

-module( problem36 ).
-export( [ main/1,
           is_base_10_palindrome/1,
           convert_base_10_int_to_base_2_string/1 ] ).

main( Upper_Limit )
->
    find_sum_of_palindromes( Upper_Limit )
.

find_sum_of_palindromes( Upper_Limit )
->
    find_sum_of_palindromes( Upper_Limit, 
                             1, 
                             0 )
.
find_sum_of_palindromes( Upper_Limit, 
                         Current, 
                         Sum ) when Current >= Upper_Limit
->
    Sum
;
find_sum_of_palindromes( Upper_Limit,
                         Current, 
                         Sum )
->
    case is_base_10_palindrome( Current ) andalso
         is_base_2_palindrome( Current )  of
         true
         ->
            find_sum_of_palindromes( Upper_Limit,
                                     Current + 1,
                                     Sum + Current )
         ;
         false
         ->
            find_sum_of_palindromes( Upper_Limit,
                                     Current + 1,
                                     Sum )
    end
.

% Convert to string and compare. 
is_base_10_palindrome( Base_10_N )
->
    String = integer_to_list( Base_10_N ),
    % Split in half, if length(String) is odd, length(Front)+1 == length(Back)
    {Front, Back} = lists:split( length(String) div 2, String ),
    % Reverse the second half of the string so we can easily compare heads
    is_palindrome( Front, lists:reverse( Back ) )
.

is_base_2_palindrome( Base_10_N )
->
    Base_2_String = convert_base_10_int_to_base_2_string( Base_10_N ),
    {Front, Back} = lists:split( length(Base_2_String) div 2, Base_2_String ),
    % Reverse the second half of the string so we can easily compare heads
    is_palindrome( Front, lists:reverse( Back ) )

.

convert_base_10_int_to_base_2_string( Base_10_N ) 
->
    convert_base_10_int_to_base_2_string( Base_10_N, [] )
.
convert_base_10_int_to_base_2_string( 0, Base_2_String )
->
    % In this problem we don't care about returning the correctly ordered 
    % binary string -- a palindrome is the same forwards as backwards after 
    % all. 
    % lists:flatten( lists:reverse(Base_2_String) )
    lists:flatten( Base_2_String )
;
convert_base_10_int_to_base_2_string( Base_10_N,
                                      Base_2_String ) when Base_10_N rem 2 == 0
->
    convert_base_10_int_to_base_2_string( Base_10_N div 2,
                                          ["0" | Base_2_String] )
;
convert_base_10_int_to_base_2_string( Base_10_N,
                                      Base_2_String ) when Base_10_N rem 2 == 1
->
    convert_base_10_int_to_base_2_string( Base_10_N div 2,
                                          ["1" | Base_2_String] )
.


is_palindrome( [], _Back )
->
    % NOTE: If length(String) is even, length(Front) = length(Back).
    %       If length(String) is odd, length(Front) + 1 = length(Back). 
    %       Thus we need to only keep track of Front. When Front is empty, 
    %       we know that we have a palindrome! 
    true
;
is_palindrome( [F_Current | _F_Rest], 
               [B_Current | _B_Rest] ) when F_Current =/= B_Current
->
    false
;
is_palindrome([_F_Current | F_Rest], 
              [_B_Current | B_Rest] )
->
    is_palindrome( F_Rest, B_Rest )
.