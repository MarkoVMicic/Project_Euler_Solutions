% Problem link: https://projecteuler.net/problem=18


% Problem Statement:
% By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

%    *3
%   *7 4
%  2 *4 6
% 8 5 *9 3

% That is, 3 + 7 + 4 + 9 = 23.

% Find the maximum total from top to bottom of the triangle below:

%                             75
%                           95  64 
%                         17  47  82
%                       18  35  87  10
%                     20  04  82  47  65
%                   19  01  23  75  03  34
%                 88  02  77  73  07  63  67
%               99  65  04  28  06  16  70  92
%             41  41  26  56  83  40  80  70  33
%           41  48  72  33  47  32  37  16  94  29
%         53  71  44  65  25  43  91  52  97  51  14
%       70  11  33  28  77  73  17  78  39  68  17  57
%     91  71  52  38  17  14  91  43  58  50  27  29  48
%   63  66  04  68  89  53  67  30  73  16  69  87  40  31
% 04  62  98  27  23  09  70  98  73  93  38  53  60  04  23

% NOTE: As there are only 16384 routes, it is possible to solve this problem 
% by trying every route. However, Problem 67, is the same challenge with a 
% triangle containing one-hundred rows; it cannot be solved by brute force, 
% and requires a clever method! ;o)

-module( problem18 ).
-export( [main/1, add_adjacent_rows/2] ).

main( Input_Path )
-> 
    case file:read_file( Input_Path ) of
        {ok, Binary}
        ->
            String = binary_to_list( Binary ),
            Number_Triangle = process_string_into_triangle( String ),
            max_path_sum( Number_Triangle )
        ;
        {error, Reason}
        ->
            {error, Reason}
    end
.

% --------------------------------------------------------------------
% Takes a string of digits, spaces and newlines, and processes them 
% into a list of lists of integers, with each row of the triangle 
% Represented by a row.
% --------------------------------------------------------------------  
process_string_into_triangle( String )
->
    process_string_into_triangle( String, [], [], [] )
.
process_string_into_triangle( [Char | Rest],
                              Number_String,
                              Row,
                              Number_Triangle ) when [Char] == " "
->
    Number = list_to_integer( lists:reverse(Number_String) ),
    process_string_into_triangle( Rest,
                                  [],
                                  [Number | Row],
                                  Number_Triangle )
;
process_string_into_triangle( [Char | Rest],
                              Number_String,
                              Row,
                              Number_Triangle ) when [Char] == "\n"
->
    Number = list_to_integer( lists:reverse(Number_String) ),
    New_Row = lists:reverse( [Number | Row] ),
    process_string_into_triangle( Rest,
                                  [],
                                  [],
                                  [New_Row | Number_Triangle] )
;
process_string_into_triangle( [Char | Rest],
                              Number_String,
                              Row,
                              Number_Triangle )
->
    process_string_into_triangle( Rest,
                                  [Char | Number_String],
                                  Row,
                                  Number_Triangle )
;
process_string_into_triangle( [],
                              Number_String, 
                              Row,
                              Number_Triangle )
->
    Number = list_to_integer( lists:reverse(Number_String) ),
    New_Row = lists:reverse( [Number | Row] ),
    % Since we want to process the triangle bottom-to-top, don't reverse here. 
    [New_Row | Number_Triangle]
.


% ----------------------------------------------------------------------
% This computes the sum of the maximum path as required by the question.
% It looks at the bottom-most row and the next row up, and then 
% sums up the max of the two adjacent elements in Current_Row with the
% Element in Next_Row.
%
% Since we do not actually need the path in this case, we can just 
% iterate this process, discarding the last row each time until we
% eventually hit the top row. Since at each iteration we take the 
% maximum of the adjacent elements, this will eventually yield the sum 
% of this path. 
%
% Technically, if you do not discard the rows, but instead preserve
% them, then you can simply walk down the triangle looking at the 
% next max adjacent element to walk to. An example is shown below.
% --------------------------------------------------------------------
max_path_sum( [Current_Row, Next_Row | Triangle] )
->
    New_Bottom_Row = add_adjacent_rows( Current_Row, Next_Row ),
    max_path_sum( [New_Bottom_Row | Triangle] )
;
max_path_sum( [Current_Row | []] )
->
    [Sum] = Current_Row,
    Sum
.

% --------------------------------------------------------------------
% This generates a new row by adding the max of ith and ith+1 element
% of the Current_Row with the ith element of Next_Row. 
% --------------------------------------------------------------------
add_adjacent_rows( Current_Row, Next_Row )
->
    add_adjacent_rows( Current_Row, 
                       Next_Row,
                       [] )
.
add_adjacent_rows( [Current_1, Current_2 | Current_Row], 
                   [Next | Next_Row], 
                   New_Row )
->
    add_adjacent_rows( [Current_2 | Current_Row],
                       Next_Row,
                       [Next + erlang:max(Current_1, Current_2) | New_Row] )
;
add_adjacent_rows( _Current_Row,
                   [],
                   New_Row )
->
    lists:reverse( New_Row )
.


% --------------------------------------------------------------------
% Example: Consider this triangle (the same as the example triangle 
%          in the question prompt)
%       3
%      / \
%     7   4
%    / \ / \
%   2   4   6
%  / \ / \ / \
% 8   5   9   3
%
% By going through the current row and adding the max of the two 
% elements to their adjacent element in the next row, we get this:
%
%         3
%       /   \
%      7     4
%    /   \ /   \
% (2+8) (4+9) (6+9)
%  /  \ /   \ /   \
% 8    5     9     3
% 
% i.e. 
% 
%       3
%      / \
%     7   4
%    / \ / \
%   10  13  15
%  / \ / \ / \
% 8   5   9   3
% 
% The next steps:
% 
%         3
%       /   \
%   (7+13) (4+15)
%    /   \ /   \
%   10   13    15
%  /  \ /  \ /   \
% 8    5    9     3
% 
%      (20+3) = 23 which is the answer
%       /   \
%     20    19
%    /   \ /  \
%   10   13    15
%  /  \ /  \ /  \
% 8    5    9    3
% 
% If we have not discarded this triangle, but modified it, then we can 
% even trace the path!
% 
%        *23
%       /   \
%    *20    19
%    /   \ /  \
%   10  *13    15
%  /  \ /  \ /  \
% 8    5   *9    3
% 
% which is the same as 
% 
%      *3
%      / \
%    *7   4
%    / \ / \
%   2  *4   6
%  / \ / \ / \
% 8   5  *9   3
% --------------------------------------------------------------------


% --------------------------------------------------------------------
% Greedy-path algorithm. Doesn't actually solve the problem though >.< 
% --------------------------------------------------------------------
% greedy_path_sum( Triangle ) -> greedy_path_sum( Triangle, 1, 1, 0 ).
% % First row: Just add the number.
% greedy_path_sum( [Current_Row| Triangle],
%                   Row_Index,
%                   _Element_Index,
%                   Acc ) when Row_Index == 1
% ->
%     [Summand] = Current_Row,
%     greedy_path_sum( Triangle,
%                      2,
%                      1,
%                      Acc + Summand )
% ;
% % Second Row: Add the max of the two numbers and check which one it is. 
% greedy_path_sum( [Current_Row | Triangle],
%                  Row_Index,
%                  _Element_Index,
%                  Acc ) when Row_Index == 2
% ->
%     [Summand_1, Summand_2] = Current_Row,
%     case Summand_1 > Summand_2 of
%         true
%         ->
%             greedy_path_sum( Triangle,
%                              3,
%                              1,
%                              Acc + Summand_1 )
%         ;
%         false
%         ->
%             greedy_path_sum( Triangle,
%                              3,
%                              2,
%                              Acc + Summand_2 )
%     end
% ;
% % Find the max of Element_Index and Element_Index+1 of the current 
% % row, add the result to Acc and keep track of the index, while incrementing 
% % the row by 1. 
% greedy_path_sum( [Current_Row | Triangle],
%                  Row_Index,
%                  Element_Index,
%                  Acc )
% ->
%     Summand_1 = lists:nth(Element_Index, Current_Row ),
%     Summand_2 = lists:nth(Element_Index + 1, Current_Row ),
%     case Summand_1 > Summand_2 of
%         true
%         ->
%             greedy_path_sum( Triangle,
%                              Row_Index + 1,
%                              Element_Index,
%                              Acc + Summand_1 )
%         ;
%         false
%         ->
%             greedy_path_sum( Triangle,
%                              Row_Index + 1,
%                              Element_Index+1,
%                              Acc + Summand_2 )
%     end
% ;
% greedy_path_sum( [],
%                  _Row_Index,
%                  _Element_Index,
%                  Acc )
% ->
%     Acc
% .
