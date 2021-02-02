% Problem link: https://projecteuler.net/problem=24


% Problem statement:
% A permutation is an ordered arrangement of objects. For example, 314 is one 
% possible permutation of the digits 1, , 3 and 4. If all of the permutations 
% are listed numerically or alphabetically, we call it lexicographic order. 
% The lexicographic permutations of 0, 1 and 2 are:

% 012   021   102   120   201   210

% What is the millionth lexicographic permutation of the digits 
% 0, 1, 2 , 3, 4, 5, 6, 7, 8 and 9?

-module( problem24 ).
-export( [ main/2 ] ).


main( Num_Digits, Index )
->
    % Generate a sorted list of integers from 0->N-1, which serves as the 
    % first permutation. 
    List = lists:seq( 0, Num_Digits-1 ),
    permute( List, Index )
.

permute( List, Max_Index )
->
    permute( List, List, [], 1, Max_Index )
.
permute( _, Permutation, _, Index, Max_Index) when Index == Max_Index 
-> 
    Permutation
;
permute( List, Permutation, Permutation_List, Index, Max_Index )
->
    case find_K( Permutation ) of
        {KIndex, KValue} 
        ->      
            IIndex = find_I( Permutation, KIndex, KValue ),
            Swapped = swap_by_index( KIndex, IIndex, Permutation ),
            New_Permutation = reverse_from_K_plus_1( Swapped, KIndex ),
            permute( List, 
                     New_Permutation, 
                     [Permutation | Permutation_List], 
                     Index+1,
                     Max_Index )
        ;
        no_K_found
        ->
            lists:reverse( [Permutation | Permutation_List] )
    end
.

% -----------------------------------------------------------------
% Step 1:
% Find largest index K such that Permutation[K] < Permutation[K+1].
% If no such K exists, then we are done with the algorithm i.e. we
% have arrived at the last permutation in lexicographical order.  
% -----------------------------------------------------------------
find_K( [First | _Rest] = Permutation )
->
    find_K( Permutation, 
            length(Permutation), 
            1, 
            length(Permutation), 
            First )
.
find_K( [_X|[]], 
        Length, 
        _Index, 
        KIndex, 
        _KValue ) when KIndex == Length
->
    no_K_found
;
find_K( [_X|[]], 
        _Length, 
        _Index, 
        KIndex, 
        KValue )
->
    {KIndex, KValue}
;
find_K( [First, Second | Rest], 
        Length, 
        Index, 
        _KIndex, 
        _KValue ) when First < Second
->
    find_K( [Second | Rest], 
        Length, 
        Index+1, 
        Index, 
        First )
;
find_K( [_First, Second | Rest], 
        Length, 
        Index, 
        KIndex, 
        KValue )
->
    find_K( [Second | Rest], 
            Length, 
            Index+1, 
            KIndex, 
            KValue )
.


% -------------------------------------------------------------------
% Step 2:
% Find largest index I > K such that Permutation[K] < Permutation[I]
% ------------------------------------------------------------------- 
find_I( Permutation, 
        KIndex, 
        _KValue ) when KIndex == (length(Permutation) - 1)
->
    % Shortcut if KIndex is the second-last index
    length( Permutation )
;
find_I( Permutation, 
        KIndex, 
        KValue )
->
    {_Discard, Trimmed_Permutation} = lists:split(KIndex, Permutation),
    find_I( Trimmed_Permutation, 
            KIndex+1, 
            KIndex+1, 
            KValue )
.
find_I( [Current | Rest], 
        Index, 
        _IIndex, 
        KValue ) when Current > KValue
->
    find_I( Rest, 
            Index+1, 
            Index, 
            KValue)
;
find_I( [_Current | Rest], 
        Index, 
        IIndex, 
        KValue )
->
    find_I( Rest, 
            Index+1, 
            IIndex, 
            KValue)
;
find_I( [], 
        _Index, 
        IIndex, 
        _KValue )
->
    IIndex
.


% ---------------------------------------
% Step 3:
% Swap Permutation[K] and Permutation[I].
% ---------------------------------------
% worked example: 
%   suppose we wish to swap the 3rd and 6th element in the list 
% 
%       [a,b,c,d,e,f,g,h]
% 
%   In Erlang, we can first split the list up from the smaller index:
% 
%       [a,b,c] [d,e,f,g,h]
% 
%   Then we can split the list up from the larger index too:
% 
%       [a,b,c] [d,e,f] [g,h]
% 
%   Take note that to split the second list, we index the second list using 
%   the difference between the larger index and the smaller index (in this 
%   case 6-3 = 3).
% 
%   Now we can reverse both the first and second sublists to gain access to 
%   the desired elements that we wish to swap. 
% 
%       [c,b,a] [f,e,d] [g,h]
%
%   Now we can extract the first element of the first two sublists using 
%   Erlang's list syntax [Head | Tail], and add them as the head of the other 
%   sublist.
%
%       [f,b,a] [c,e,d] [g,h]
%
%   Then we reverse the first two sublists and concatenate all three sublists 
%   together
%
%       [a,b,f] ++ [d,e,c] ++ [g,h] = [a,b,f,d,e,c,g,h]
%
swap_by_index( Index1, Index2, List ) when Index1 == Index2
->
    List
;
swap_by_index( Index1, Index2, List )
->
    Split1 = min(Index1, Index2),
    Split2 = max(Index1, Index2),
    {SubList1, Next} = lists:split(Split1, List),
    {SubList2, SubList3} = lists:split(Split2 - Split1, Next),
    [First_Element | Rev_Trimmed_SubList1] = lists:reverse(SubList1),
    [Second_Element | Rev_Trimmed_SubList2] = lists:reverse(SubList2),
    lists:reverse([Second_Element | Rev_Trimmed_SubList1]) ++ 
    lists:reverse([First_Element | Rev_Trimmed_SubList2]) ++ SubList3
.


% -------------------------------------------------------------------
% Step 4:
% Reverse the order of all elements in the sublist Permutation[K+1]
% to Permutation[N]
% -------------------------------------------------------------------
reverse_from_K_plus_1( List, KIndex )
->
    {Head_List, Tail_List} = lists:split( KIndex, List ),
    Head_List ++ lists:reverse( Tail_List )
.
