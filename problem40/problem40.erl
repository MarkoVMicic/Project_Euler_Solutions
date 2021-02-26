% Problem link: https://projecteuler.net/problem=40


% Problem statement:
% An irrational decimal fraction is created by concatenating the positive 
% integers:

% 0.123456789101112131415161718192021...

% It can be seen that the 12th digit of the fractional part is 1.

% If d_n represents the nth digit of the fractional part, find the value of 
% the following expression.

% d_1 × d_10 × d_100 × d_1000 × d_10000 × d_100000 × d_1000000

-module( problem40 ).
-export( [ main/2,
           create_champernowne_string/1 ] ).

% NOTE: Max_Index is used to indicate how many integers we are concatenating. 
%       Index_Multiple is used to indicate the geometric growth of the indices 
%       we care about. In the problem as stated, Index_Multiple is 10
main( Max_Index, Index_Multiple )
->
    compute_product( Max_Index, Index_Multiple )
.


compute_product( Max_Index, Index_Multiple )
->
    String_Champernowne_Constant = create_champernowne_string( Max_Index ),
    compute_product( Max_Index, 
                     Index_Multiple, 
                     String_Champernowne_Constant, 
                     1, 
                     1,
                     [] )
.
% When Current_Index > Max_Index, we are done
compute_product( Max_Index, 
                 _Index_Multiple,
                 _Digit_String,
                 Current_Index,
                 _Next_Index,
                 Multiplicands ) when Current_Index > Max_Index
->
    multiply_multiplicands( Multiplicands )
;
% When Current_Index == Next_Index, we are at a digit that we wish to add to 
% the list of multiplicands. Add it to the list, and increment Next_Index by 
% multiplying it with Index_Multiple. 
compute_product( Max_Index, 
                 Index_Multiple,
                 [Current_Digit | Rest],
                 Current_Index,
                 Next_Index,
                 Multiplicands ) when Current_Index == Next_Index
->
    compute_product( Max_Index, 
                     Index_Multiple,
                     Rest,
                     Current_Index+1,
                     Next_Index * Index_Multiple,
                     [Current_Digit | Multiplicands] )
;
% Default case is to just go to the next digit in the list.
compute_product( Max_Index, 
                 Index_Multiple,
                 [_Current_Digit | Rest],
                 Current_Index,
                 Next_Index,
                 Multiplicands ) 
->
    compute_product( Max_Index, 
                     Index_Multiple,
                     Rest,
                     Current_Index+1,
                     Next_Index,
                     Multiplicands )
.


% -----------------------------------------------------------------------------
% Create a string representing Champernowne's Constant by concatenating all 
% integers from 1 to Max_Index. Return it as a string to parse through 
% individual digits easier. 
% -----------------------------------------------------------------------------
create_champernowne_string( Max_Index )
->
    create_champernowne_string( lists:seq(1, Max_Index), [] )
.
create_champernowne_string( [Current_Number | Rest], String )
->
    Number_String = integer_to_list( Current_Number ),
    create_champernowne_string( Rest, [ Number_String | String ] )
;
create_champernowne_string( [], String )
->
    lists:flatten( lists:reverse(String) )
.


% -----------------------------------------------------------------------------
% Multiply the elements in the list. Note that the list is a string, so we 
% need to call list_to_integer() before multiplying. 
% -----------------------------------------------------------------------------
multiply_multiplicands( Multiplicands )
->
    multiply_multiplicands( Multiplicands, 1 )
.
multiply_multiplicands( [Digit | Rest], Acc )
->
    multiply_multiplicands( Rest, list_to_integer([Digit]) * Acc )
;
multiply_multiplicands( [], Acc )
->
    Acc
.
