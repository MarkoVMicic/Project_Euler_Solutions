% Problem link: https://projecteuler.net/problem=28


% Problem Statement:
% Starting with the number 1 and moving to the right in a clockwise direction 
% a 5 by 5 spiral is formed as follows:

% *21*  22  23  24 *25*
%  20  *7*  8  *9*  10
%  19   6  *1*  2   11
%  18  *5*  4  *3*  12
% *17*  16  15  14 *13*

% It can be verified that the sum of the numbers on the diagonals is 101.

% What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral 
% formed in the same way?

-module( problem28 ).
-export( [ main/1 ] ).

main( N )
->
    case N rem 2 == 0 of
        true
        ->
            Upper_Right = even_N_sum_upper_right_diagonal( N ),
            Upper_Left  = even_N_sum_upper_left_diagonal( N ),
            Lower_Left  = even_N_sum_lower_left_diagonal( N ),
            Lower_Right = even_N_sum_lower_right_diagonal( N ),
            Upper_Right + Upper_Left + Lower_Left + Lower_Right
        ;
        false
        ->
            Upper_Right = odd_N_sum_upper_right_diagonal( N ),
            Upper_Left  = odd_N_sum_upper_left_diagonal( N ),
            Lower_Left  = odd_N_sum_lower_left_diagonal( N ),
            Lower_Right = odd_N_sum_lower_right_diagonal( N ),
            % Add the central 1 at the end 
            Upper_Right + Upper_Left + Lower_Left + Lower_Right + 1
    end
.

% --------------------
% N is an even Number
% --------------------
% In an even-square (i.e. a square whose side N is an even number), the upper 
% right diagonal consists of odd N^2 + 1 up to N .
%
% e.g. N = 6:
% 
%       24  25 (26)  --> 5^2 + 1
%       9  (10) 27   --> 3^2 + 1
%      (2)  11  26   --> 1^2 + 1

even_N_sum_upper_right_diagonal( N )
->
    even_N_sum_upper_right_diagonal( N-1, 1, 0 )
.
even_N_sum_upper_right_diagonal( N_minus_1, 
                                Current, 
                                Sum ) when Current == N_minus_1
->
    Sum + Current*Current + 1
;
even_N_sum_upper_right_diagonal( N_minus_1, 
                                Current, 
                                Sum )
->
    even_N_sum_upper_right_diagonal( N_minus_1, 
                                    Current + 2, 
                                    Sum + Current*Current + 1 )
.

% In an even-square (i.e. a square whose side N is an even number), the upper 
% left diagonal consists of (odd N^2 - (N-1)) up to N-1.
%
% e.g. N = 6:
%
%        (21) 22  23  --> 5^2 - 4
%         20 (7)  8   --> 3^2 - 2
%         19  6  (1)  --> 1^2 - 0


even_N_sum_upper_left_diagonal( N )
->
    even_N_sum_upper_left_diagonal( N-1, 1, 0 )
.
even_N_sum_upper_left_diagonal( N_minus_1, 
                               Current, 
                               Sum ) when Current == N_minus_1
->
    Sum + (Current*Current - (Current-1))
;
even_N_sum_upper_left_diagonal( N_minus_1, 
                               Current, 
                               Sum )
->
    even_N_sum_upper_left_diagonal( N_minus_1, 
                                   Current + 2, 
                                   Sum + (Current*Current - (Current-1)) )
.


% In an even-square (i.e. a square whose side N is an even number), the lower
% left diagonal consists of even N^2 up to N
%
% e.g. N = 6:
% 
%       18  5  (4)  --> 2^2
%       17 (16) 15  --> 4^2
%      (36) 35  34  --> 6^2
even_N_sum_lower_left_diagonal( N )
->
    even_N_sum_lower_left_diagonal( N, 2, 0 )
.
even_N_sum_lower_left_diagonal( N,
                               Current,
                               Sum ) when Current == N
->
    Sum + Current*Current
;
even_N_sum_lower_left_diagonal( N,
                               Current,
                               Sum ) 
->
    even_N_sum_lower_left_diagonal( N,
                                    Current + 2,
                                    Sum + Current*Current)
.


% In an even-square (i.e. a square whose side N is an even number), the lower
% right diagonal consists of even (N^2 - (N-1)) up to N
%
% e.g. N = 6:
% 
% 
%       (3)  12  29  --> 2^2 - 1
%        14 (13) 30  --> 4^2 - 3
%        33  32 (31) --> 6^2 - 5
even_N_sum_lower_right_diagonal( N )
->
    even_N_sum_lower_right_diagonal( N, 2, 0 )
.
even_N_sum_lower_right_diagonal( N,
                                Current,
                                Sum ) when Current == N
->
    Sum + Current*Current - (Current-1)
;
even_N_sum_lower_right_diagonal( N,
                                Current,
                                Sum )
->
    even_N_sum_lower_right_diagonal( N,
                                    Current + 2,
                                    Sum + Current*Current - (Current-1) )
.


% ------------------
% N is an odd Number
% ------------------
% In an odd-square (i.e. a square whose side N is an odd number), the upper 
% right diagonal (excluding the middle 1) consists of odd squares up to N.
%
% e.g. N = 7:
% 
%       46  47  48 (49) --> 7^2    
%       23  24 (25) 26  --> 5^2
%       8  (9)  10  27  --> 3^2
%       1   2   11  26
%       ^
%       |
%    excluded (summed in right at the end)

odd_N_sum_upper_right_diagonal( N )
->
    odd_N_sum_upper_right_diagonal( N, 3, 0 )
.
odd_N_sum_upper_right_diagonal( N, 
                                Current, 
                                Sum ) when Current == N
->
    Sum + Current*Current
;
odd_N_sum_upper_right_diagonal( N, 
                                Current, 
                                Sum )
->
    odd_N_sum_upper_right_diagonal( N, 
                                    Current + 2, 
                                    Sum + Current*Current )
.

% In an odd-square (i.e. a square whose side N is an odd number), the upper 
% left diagonal (excluding the middle 1) consists of (odd squares - 1 less 
% than the odd number) up to N^2.
%
% e.g. N = 7:
%
%       (43) 44  45  46  --> 7^2 - 6
%        42 (21) 22  23  --> 5^2 - 4
%        41  20 (7)  8   --> 3^2 - 2
%        40  19  6   1   
%                    ^
%                    |
%          excluded (summed in right at the end)

odd_N_sum_upper_left_diagonal( N )
->
    odd_N_sum_upper_left_diagonal( N, 3, 0 )
.
odd_N_sum_upper_left_diagonal( N, 
                               Current, 
                               Sum ) when Current == N
->
    Sum + (Current*Current - (Current-1))
;
odd_N_sum_upper_left_diagonal( N, 
                               Current, 
                               Sum )
->
    odd_N_sum_upper_left_diagonal( N, 
                                   Current + 2, 
                                   Sum + (Current*Current - (Current-1)) )
.


% In an odd-square (i.e. a square whose side N is an odd number), the lower
% left diagonal (excluding the middle 1) consists of (even squares + 1) up to 
% N-1
%
% e.g. N = 7:
% 
%           excluded (summed in right at the end)
%                   |
%                   v
%       40  19  6   1
%       39  18 (5)  4   --> 2^2 + 1
%       38 (17) 16  15  --> 4^2 + 1
%      (37) 36  35  34  --> 6^2 + 1
odd_N_sum_lower_left_diagonal( N )
->
    odd_N_sum_lower_left_diagonal( N-1, 2, 0 )
.
odd_N_sum_lower_left_diagonal( N_minus_1,
                               Current,
                               Sum ) when Current == N_minus_1
->
    Sum + Current*Current + 1
;
odd_N_sum_lower_left_diagonal( N_minus_1,
                               Current,
                               Sum ) 
->
    odd_N_sum_lower_left_diagonal( N_minus_1, 
                                    Current + 2,
                                    Sum + Current*Current + 1)
.


% In an odd-square (i.e. a square whose side N is an odd number), the lower
% right diagonal (excluding the middle 1) consists of (even squares - (N-1)) 
% up to N-1
%
% e.g. N = 7:
% 
% 
% excluded (summed in right at the end)
%       |
%       v
%       1   2   11  28
%       4  (3)  12  29  --> 2^2 - 1
%       15  14 (13) 30  --> 4^2 - 3
%       34  33  32 (31) --> 6^2 - 5
odd_N_sum_lower_right_diagonal( N )
->
    odd_N_sum_lower_right_diagonal( N-1, 2, 0 )
.
odd_N_sum_lower_right_diagonal( N_minus_1,
                                Current,
                                Sum ) when Current == N_minus_1
->
    Sum + Current*Current - (Current-1)
;
odd_N_sum_lower_right_diagonal( N_minus_1,
                                Current,
                                Sum )
->
    odd_N_sum_lower_right_diagonal( N_minus_1,
                                    Current + 2,
                                    Sum + Current*Current - (Current-1) )
.