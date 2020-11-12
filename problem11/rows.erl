-module(rows).
-export([build_rows/2, reverse_rows/1, flatten_rows/1]).

% --------------------------------------------------------------
% Convert list of 2-digit numbers into list of rows aka 2D-array 
% --------------------------------------------------------------
build_rows( List, Side_Length ) -> build_rows( List, Side_Length, [], [] ).
build_rows( List, 
            Side_Length, 
            Row, 
            Grid ) when length(Row) >= Side_Length
->
    build_rows( List, 
                Side_Length, 
                [], 
                [lists:reverse(Row) | Grid] )
;
build_rows( [Number | Rest],
            Side_Length,
            Row,
            Grid )
->
    build_rows( Rest,
                Side_Length,
                [Number | Row],
                Grid )
;
build_rows( [],
            _Side_Length,
            [],
            Grid )
->
    lists:reverse(Grid)
.

% -------------------------------------
% For a list of Rows, reverse each row. 
% -------------------------------------
reverse_rows( Rows ) -> reverse_rows( Rows, [], [] ).
reverse_rows( [[Number | Row] | Rows], Reverse_Row, Reverse_Grid )
->
    reverse_rows( [Row | Rows], [Number | Reverse_Row], Reverse_Grid )
;
reverse_rows( [[] | Rows], Reverse_Row, Reverse_Grid )
->
    reverse_rows( Rows, [], [Reverse_Row | Reverse_Grid] )
;
reverse_rows( [], [], Reverse_Grid )
->
    lists:reverse( Reverse_Grid )
.

% Need to use this because lists:flatten flattens the string elements into one 
% long string! 
flatten_rows( Rows ) -> flatten_rows( Rows, [] ).
flatten_rows( [[Number | []] | Rows], Flat_Grid )
->
    flatten_rows( Rows, [Number | Flat_Grid] )
;
flatten_rows( [[Number | Row] | Rows], Flat_Grid )
->
    flatten_rows( [Row | Rows], [Number | Flat_Grid] )
;
flatten_rows( [], Flat_Grid )
->
    lists:reverse( Flat_Grid )
.

