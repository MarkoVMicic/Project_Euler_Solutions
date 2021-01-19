% Problem link: https://projecteuler.net/problem=22


% Problem Statement: 
% Using names.txt, a 46K text file containing over five-thousand first names, 
% begin by sorting it into alphabetical order. Then working out the 
% alphabetical value for each name, multiply this value by its alphabetical 
% position in the list to obtain a name score.

% For example, when the list is sorted into alphabetical order, COLIN, which 
% is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN 
% would obtain a score of 938 × 53 = 49714.

% What is the total of all the name scores in the file?

-module(problem22).
-export([main/1]).

main( Input_Path )
->
    case file:read_file( Input_Path ) of
        {ok, Binary}
        ->
            String = binary_to_list( Binary ),
            List = string:tokens( String, "," ),
            Sorted_List = lists:sort( List ),
            compute_name_score( Sorted_List )
        ;
        {error, Reason}
        ->
            {error, Reason}
    end
.

compute_name_score( Sorted_List )
->
    compute_name_score( Sorted_List, create_alphabet_map(), 1, 0 )
.
compute_name_score( [], _Map, _Index, Acc )
->
    Acc
;
compute_name_score( [Name | Rest], Map, Index, Acc )
->
    Sum = compute_name_sum( Name, Map ),
    compute_name_score( Rest, Map, Index + 1, Acc + (Sum*Index) )
.

compute_name_sum( Name, Map )
->
    compute_name_sum( Name, Map, 0 )
.
compute_name_sum( [], _Map, Sum )
->
    Sum
;
compute_name_sum( [Char | Rest], Map, Sum )
->
    case Map of
        #{ [Char] := Score } -> compute_name_sum( Rest, Map, Sum+Score );
        _                    -> throw(error)
    end
.

create_alphabet_map()
->
    #{ 
        "\"" =>  0,
        "A"  =>  1,
        "B"  =>  2,
        "C"  =>  3,
        "D"  =>  4,
        "E"  =>  5,
        "F"  =>  6,
        "G"  =>  7,
        "H"  =>  8,
        "I"  =>  9,
        "J"  =>  10,
        "K"  =>  11,
        "L"  =>  12,
        "M"  =>  13,
        "N"  =>  14,
        "O"  =>  15,
        "P"  =>  16,
        "Q"  =>  17,
        "R"  =>  18,
        "S"  =>  19,
        "T"  =>  20,
        "U"  =>  21,
        "V"  =>  22,
        "W"  =>  23,
        "X"  =>  24,
        "Y"  =>  25,
        "Z"  =>  26
    }
.
