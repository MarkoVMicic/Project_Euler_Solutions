-module(prod).
-export([find_largest_product/2]).

% Iterate through list of lists, and for each list find the largest product of 
% N adjacent elements. For each List in the list of lists, this function 
% returns 0 if N > length(List)
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
