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
-export( [ main/1] ).

main( N )
->
    sum_diagonals_of_spiral( N )
.


sum_diagonals_of_spiral( 1 ) -> 1;
sum_diagonals_of_spiral( 2 ) -> 10;
sum_diagonals_of_spiral( N ) when N rem 2 == 0
-> 
    % N is even
    sum_diagonals_of_spiral( N, 2, 0 )
;
sum_diagonals_of_spiral( N ) 
-> 
    % N is odd -- start at K=3 and initialize sum to 1! See comment at end. 
    sum_diagonals_of_spiral( N, 3, 1 )
.
sum_diagonals_of_spiral( N, Current, Sum) when Current > N
->
    Sum
;
sum_diagonals_of_spiral( N , Current, Sum )
->
    % See comment at end for derivation of this formula. 
    sum_diagonals_of_spiral( N, 
                             Current + 2,
                             Sum + 4*Current*Current - 6*Current + 6 )
.

% There are 4 corners of each spiral. Each corner can be computed from the 
% size of the spiral. For an K-sided spiral, the four corners can be described 
% as
% 
%   K^2
%   K^2 - (K-1)
%   (K-1)^2 + 1
%   (K-1)^2 - ((K-1) - 1) 
% 
% Simplifying this yields
% 
%  4K^2 - 6K + 6
% 
% Thus, beginning at 1, we simply grow the spiral side by 2 each step until we 
% hit N, and at each stage K we sum the corners. This will automatically sum 
% the diagonals by the time we've grown the spiral to N. 
%
% Note that the sum_diagonals formula produces 4 instead of 1 when K = 1, so 
% we must handle this exceptional case. We also have to check if N is even or 
% odd. We can handle both these cases by beginning at K=2 for even N and K=3 
% for odd N (taking care to initialize sum to 1 in the latter case to account 
% for K=1 case where the sum of the diagonals is 1).