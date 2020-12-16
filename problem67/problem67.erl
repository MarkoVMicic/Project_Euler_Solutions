% Problem link: https://projecteuler.net/problem=67


% Problem Statement:
% By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

%    *3
%   *7 4
%  2 *4 6
% 8 5 *9 3

% That is, 3 + 7 + 4 + 9 = 23.

% Find the maximum total from top to bottom in triangle.txt, a 15K text file 
% containing a triangle with one-hundred rows.

% NOTE: This is a much more difficult version of Problem 18. It is not 
% possible to try every route to solve this problem, as there are 299 
% altogether! If you could check one trillion (1012) routes every second it 
% would take over twenty billion years to check them all. There is an 
% efficient algorithm to solve it. ;o)

-module(problem67).
-export([main/1]).

% See problem18.erl for extensive documentation. 

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
% This clause catches an empty line.
process_string_into_triangle( [], 
                              Number_String, 
                              _Row, 
                              Number_Triangle ) when length(Number_String) == 0
->
    Number_Triangle
;
process_string_into_triangle( [],
                              Number_String, 
                              Row,
                              Number_Triangle )
->
    Number = list_to_integer( lists:reverse(Number_String) ),
    New_Row = lists:reverse( [Number | Row] ),
    [New_Row | Number_Triangle]
.

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
