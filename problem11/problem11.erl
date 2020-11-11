% Problem link: https://projecteuler.net/problem=11


% Problem statement:

% In the 20×20 grid below, four numbers along a diagonal line have been marked 
% in red.

% 08 02 22 97 38 15 00 40  00   75   04   05  07 78 52 12 50 77 91 08
% 49 49 99 40 17 81 18 57  60   87   17   40  98 43 69 48 04 56 62 00
% 81 49 31 73 55 79 14 29  93   71   40   67  53 88 30 03 49 13 36 65
% 52 70 95 23 04 60 11 42  69   24   68   56  01 32 56 71 37 02 36 91
% 22 31 16 71 51 67 63 89  41   92   36   54  22 40 40 28 66 33 13 80
% 24 47 32 60 99 03 45 02  44   75   33   53  78 36 84 20 35 17 12 50
% 32 98 81 28 64 23 67 10 *26*  38   40   67  59 54 70 66 18 38 64 70
% 67 26 20 68 02 62 12 20  95  *63*  94   39  63 08 40 91 66 49 94 21
% 24 55 58 05 66 73 99 26  97   17  *78*  78  96 83 14 88 34 89 63 72
% 21 36 23 09 75 00 76 44  20   45   35  *14* 00 61 33 97 34 31 33 95
% 78 17 53 28 22 75 31 67  15   94   03   80  04 62 16 14 09 53 56 92
% 16 39 05 42 96 35 31 47  55   58   88   24  00 17 54 24 36 29 85 57
% 86 56 00 48 35 71 89 07  05   44   44   37  44 60 21 58 51 54 17 58
% 19 80 81 68 05 94 47 69  28   73   92   13  86 52 17 77 04 89 55 40
% 04 52 08 83 97 35 99 16  07   97   57   32  16 26 26 79 33 27 98 66
% 88 36 68 87 57 62 20 72  03   46   33   67  46 55 12 32 63 93 53 69
% 04 42 16 73 38 25 39 11  24   94   72   18  08 46 29 32 40 62 76 36
% 20 69 36 41 72 30 23 88  34   62   99   69  82 67 59 85 74 04 36 16
% 20 73 35 29 78 31 90 01  74   31   49   71  48 86 81 16 23 57 05 54
% 01 70 54 71 83 51 54 69  16   92   33   48  61 43 52 01 89 19 67 48

% The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

% What is the greatest product of four adjacent numbers in the same direction 
% (up, down, left, right, or diagonally) in the 20×20 grid?

-module(problem11).
-export([main/2]).

% Key assumption: pass in one long string as the Numbers argument, and parse 
% it every 2 digits to form the new list. 
main( Numbers, N )
->
    Numbers_Length = length(Numbers) div 2,
    Square_Length   = trunc(math:sqrt(Numbers_Length)),

    % Check that the supplied number can indeed be formed into a grid. 
    case Square_Length*Square_Length == Numbers_Length of
        true 
        ->
            DD_Numbers = double_digit_list( Numbers ),
            Grid = build_grid( DD_Numbers, Square_Length ),
            % Grid already expressed in Rows
            Rows = Grid,
            Columns = build_columns( Grid ),
            % Up as in going up from left to right. 
            Up_Diag = build_up_diagonals( Grid, Square_Length )
            % Row_Product = find_largest_product( Rows, N ),
            % Column_Product = find_largest_product( Columns, N ),
            % {Row_Product, Column_Product}
        ;

        false 
        -> 
            {error, "Supplied number does not fit into a grid"}
    end


.

double_digit_list( Numbers ) -> double_digit_list( Numbers, [] ).
double_digit_list( [First, Second | Rest], New_List )
->
    double_digit_list( Rest, [[First, Second] | New_List] )
;
double_digit_list( [], New_List ) -> lists:reverse(New_List). 

build_grid( List, Side_Length ) -> build_grid( List, Side_Length, [], [] ).
build_grid( List, 
            Side_Length, 
            Row, 
            Grid ) when length(Row) >= Side_Length
->
    build_grid( List, 
                Side_Length, 
                [], 
                [lists:reverse(Row) | Grid] )
;
build_grid( [Number | Rest],
            Side_Length,
            Row,
            Grid )
->
    build_grid( Rest,
                Side_Length,
                [Number | Row],
                Grid )
;
build_grid( [],
            _Side_Length,
            [],
            Grid )
->
    lists:reverse(Grid)
.

build_columns( Grid ) 
-> 
    build_columns( Grid, [], [], [])
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

build_up_diagonals( Grid, Square_Length ) 
-> 
    build_up_diagonals( Grid, (2*Square_Length - 1), 1, 1, [], [], [] )
.
build_up_diagonals( [[Number | Row] | Rest], 
                    Num_Rows, 
                    X, 
                    Y, 
                    Temp_Grid, 
                    Diagonal, 
                    Diagonal_List )
->

.



find_largest_product( Lists, N ) -> find_largest_product( Lists, N, 0 ).
find_largest_product( [List|Rest], N, Acc )
->
    Product = largest_product_in_list( List, [], N, fill, 0, 1 ),
    case Product >= Acc of
        true  -> find_largest_product( Rest, N, Product );
        false -> find_largest_product( Rest, N, Acc )
    end
;
find_largest_product( [], _N, Acc ) -> Acc.

largest_product_in_list( [],
                      Prod_List,
                      N,
                      _Status,
                      Acc,
                      _Prod ) when length(Prod_List) < N
->
    Acc
;
largest_product_in_list( [Number | Rest], 
                         Prod_List, 
                         N, 
                         fill,
                         Acc,
                         Prod ) when length(Prod_List) < N
->
    largest_product_in_list( Rest, 
                             [Number | Prod_List], 
                             N, 
                             fill, 
                             Acc,
                             Prod )
;
largest_product_in_list( Number_List,
                         Prod_List,
                         N,
                         fill,
                         Acc,
                         _Prod ) when length(Prod_List) == N
->
    largest_product_in_list( Number_List,
                             Prod_List,
                             N,
                             compute,
                             Acc,
                             1 )
;
largest_product_in_list( Number_List, 
                         [Product | Rest],
                         N,
                         compute,
                         Acc,
                         Prod )
->
    largest_product_in_list( [Product | Number_List],
                             Rest,
                             N,
                             compute,
                             Acc,
                             Prod*list_to_integer(Product) )
;
largest_product_in_list( Number_List,
                         [],
                         N,
                         compute,
                         Acc,
                         Prod )
->
    largest_product_in_list( Number_List,
                             [],
                             N,
                             check,
                             Acc,
                             Prod )
;
largest_product_in_list( Number_List,
                         [],
                         N,
                         check,
                         Acc,
                         Prod ) when Prod > Acc
->
    largest_product_in_list( Number_List,
                             [],
                             N,
                             next,
                             Prod,
                             1)
;
largest_product_in_list( Number_List,
                         [],
                         N,
                         check,
                         Acc,
                         _Prod )
->
    largest_product_in_list( Number_List,
                             [],
                             N,
                             next,
                             Acc,
                             1)
;
largest_product_in_list( [_Number | Rest],
                         [],
                         N,
                         next,
                         Acc,
                         1 )
->
    largest_product_in_list( Rest, 
                             [],
                             N,
                             fill,
                             Acc,
                             1 )
.
