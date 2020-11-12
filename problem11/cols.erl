-module(cols).
-export([build_columns/1]).

% ---------------------------------------------------------
% Convert list of rows into list of columns aka transposing 
% ---------------------------------------------------------
% Note: Using rows as input is easier than passing the flat-list of 2-digit 
%       numbers
build_columns( Rows ) 
-> 
    build_columns( Rows, [], [], [])
.
build_columns( [ [Number | Row] | Rest ], 
               Temp_Grid,
               Column,
               Column_List )
->
    build_columns( Rest, 
                   [Row | Temp_Grid],
                   [Number | Column],
                   Column_List )
;
build_columns( [],
               Temp_Grid,
               Column,
               Column_List )
->
    build_columns( lists:reverse(Temp_Grid), 
                   [],
                   [],
                   [lists:reverse(Column) | Column_List] )
;
build_columns( [[]|_],
               _,
               _,
               Column_List )
->
    lists:reverse( Column_List )
.