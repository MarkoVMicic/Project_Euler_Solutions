-module(diag).
-export([build_diagonals/2]).

% -------------------------------------------------------
% Convert list of rows numbers into list of up-diagonals 
% -------------------------------------------------------
%
% Named "up-diagonals" because from left-to-right the numbers move down-to-up.
%
% Example: Square_Length = 6
%
%  [01  02  03  04  05  06]       [01]
%  [07  08  09  10  11  12]       [02 07]
%  [13  14  15  16  17  18]   ->  [03 08 13]
%  [19  20  21  22  23  24]       [04 09 14 19]
%  [25  26  27  28  29  30]       [05 10 15 20 25]
%  [31  32  33  34  35  36]       [06 11 16 21 16 31]
%                                 [12 17 22 27 32]
%                                 [18 23 28 33]
%                                 [24 29 34]
%                                 [30 35]
%                                 [36]
%
% To build "down-diagonals" (which are what people usually think of when they 
% think of diagonals) simply pass in reversed rows!
% 
% Example: Square_Length = 6
%
%  [06  05  04  03  02  01]       [06]
%  [12  11  10  09  08  07]       [05 12]
%  [18  17  16  15  14  13]   ->  [04 11 18]
%  [24  23  22  21  20  19]       [03 10 17 24]
%  [30  29  28  27  26  25]       [02 09 16 23 30]
%  [36  35  34  33  32  31]       [01 08 15 22 29 36]
%                                 [07 14 21 18 35]
%                                 [13 20 27 34]
%                                 [19 26 33]
%                                 [25 32]
%                                 [31]
build_diagonals( Rows, Square_Length ) 
-> 
    Flat_Grid     = rows:flatten_rows( Rows ),
    Map           = create_grid_map( Flat_Grid, 1, #{} ),
    Diagonal_List = diagonal_indices( Square_Length ),
    make_diagonals( Diagonal_List, Map, [[]] )
.

% ---------------------------------------------------
% Use erlang map to index the grid elements. 
% ---------------------------------------------------
% 
% Example: Square_Length = 3
% As rows, we have
% [a, b, c]
% [d, e, f]
% [g, h, i]
%
% Which we have flattened into the list [a, b, c, d, e, f, g, h, i]
% and we then create the map
% #{ 1 => a,
%    2 => b,
%    3 => c,
%    4 => d,
%    5 => e,
%    6 => f,
%    7 => g,
%    8 => h,
%    9 => i,
% }
create_grid_map( [Element | List], Index, Map ) 
-> 
    create_grid_map( List, Index+1, Map#{Index => Element } )
;
create_grid_map( [], _Index, Map )   -> Map.

% -------------
% GRID Indexing
% -------------
% 
% Example: Square_Length = 6, Offset = 5
%
%  01  02  03  04  05  06  
%  07  08  09  10  11  12
%  13  14  15  16  17  18
%  19  20  21  22  23  24
%  25  26  27  28  29  30
%  31  32  33  34  35  36
%
%
%  01 --------------------+
%  02, 07                 |
%  03, 08, 13             |---> left
%  04, 09, 14, 19,        |
%  05, 10, 15, 20, 25 ----+
%  06, 11, 16, 21, 26, 31  ---> mid 
%  12, 17, 22, 27, 32 ----+
%  18, 23, 28, 33         |
%  24, 29, 34             |---> right
%  30, 35                 |
%  36 --------------------+
diagonal_indices( Square_Length ) when Square_Length > 1 
-> 
    L = left( 1, Square_Length-1, [] ),
    M = mid( Square_Length, Square_Length-1, Square_Length-1, [] ),
    R = right( Square_Length, 2, Square_Length-1, Square_Length-1, [] ),
    L ++ M ++ R
;
diagonal_indices( 1 )            
-> 
    [ [1] ]
;
diagonal_indices( _ )            
-> 
    throw( badrg )
.    

mid( N, _, 0, Out )      -> [[N|Out]];
mid( N, Offset, C, Out ) -> mid( N, Offset, C-1, [N+C*Offset|Out] ).
 
left( I, Offset, Out ) when I =< Offset     -> left( I+1, Offset, [do_class(I, I, Offset, [])|Out] );
left( _, _, Out )                           -> lists:reverse( Out ).

right( _N, _M, 0, _Offset, Out ) -> lists:reverse( Out );
right( N, M, ECnt, Offset, Out ) -> right( N, M+1, ECnt-1, Offset, [do_class(N*M, ECnt, Offset, [])|Out] ).


do_class( _, 0, _Off, Out ) -> lists:reverse( Out );
do_class( I, C, Off, Out )  -> do_class( I+Off, C-1, Off, [I|Out] ).

% ----------------------------------------------------------------------------
% Use the diagonal indices and the grid-map to assemble the list of diagonals. 
% ----------------------------------------------------------------------------
make_diagonals( [[I|IL]|L], MAP, [Cls|Out] )
->
    case MAP of
        #{I := V} -> make_diagonals( [IL|L], MAP, [[V|Cls]|Out] );
        _         -> make_diagonals( [IL|L], MAP, [Cls|Out] )
    end
;
make_diagonals( [[]|L], MAP, [Cls|Out] ) 
-> 
    make_diagonals( L, MAP, [[], lists:reverse(Cls)|Out] )
; 
make_diagonals( [], _, [[]|Out] )        -> lists:reverse( Out ).
