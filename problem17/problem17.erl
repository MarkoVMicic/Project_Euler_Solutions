% Problem link: https://projecteuler.net/problem=17


% Problem Statement:
% If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
% then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.


% If all the numbers from 1 to 1000 (one thousand) inclusive were written out 
% in words, how many letters would be used?


% NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and 
% forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 
% letters. The use of "and" when writing out numbers is in compliance with 
% British usage.

-module(problem17).
-export([main/1]).

main( N )
->
    Numbers_Map = create_numbers_map(),
    Numbers_List = lists:seq( 1, N ),
    count_letters( Numbers_List, Numbers_Map )
.

% ---------------------------------------------------
% Iterate through numbers and build string from each.
% ---------------------------------------------------
count_letters( List, Map ) -> count_letters( List, Map, [] ).
count_letters( [Number | Rest], Map, String_List )
->
    Numeral = create_numeral( Number, Map ),
    count_letters( Rest, Map, [Numeral| String_List] ) 
;
count_letters( [], _Map, String_List )
->
    length( lists:flatten(String_List) )
.

% ------------------------------------
% Use Map to create string from number
% ------------------------------------
% If we arrive at any number less than 20, we have a direct value in the map
create_numeral( Number, Map ) when Number =< 20
->
    case Map of
        #{Number := String} -> String;
        _                   -> throw(numerical_error)
    end
;
% Process 10s not immediately found in map. 
create_numeral( Number, Map ) when Number < 100,
                                   Number > 20
->
    % Split into tens and units, then check map.
    [Ten, Unit] = integer_to_list( Number ),
    Tens  = list_to_integer( [Ten] ) * 10,
    Units = list_to_integer( [Unit] ),
    Ten_String = case Map of
        #{Tens := TNumeral} -> TNumeral;
        _                   -> throw(numerical_error)
    end,
    Unit_String = case Map of
        #{Units := UNumeral} -> UNumeral;
        _                    -> throw(numerical_error) 
    end,
    Number_String = [Ten_String] ++ [Unit_String],
    lists:flatten( Number_String )
;
% Process 100s
create_numeral( Number, Map ) when Number >= 100,
                                   Number < 1000
->
    [Hundred | Ten_and_Unit] = integer_to_list( Number ),
    Hundreds = list_to_integer( [Hundred] ),
    Tens_and_Units = list_to_integer( Ten_and_Unit ),
    Hundred_String1 =   
        case Map of
            #{Hundreds := HNumeral} -> lists:flatten([HNumeral]++["hundred"]);
            _                       -> throw(numerical_error)
        end,
    % Check to see if we are exactly on a hundred. If so, no need for the "and"
    Hundred_String2 = 
        case Tens_and_Units == 0 of
            true  -> "";
            false -> "and"
        end,
    Hundred_String = lists:flatten( [Hundred_String1] ++ [Hundred_String2] ), 
    % Process the tens and units here
    Tens_and_Units_String = create_numeral( Tens_and_Units, Map ),
    Number_String = [Hundred_String] ++ [Tens_and_Units_String],
    lists:flatten( Number_String )
;
create_numeral( Number, Map ) when Number == 1000
->
    case Map of 
        #{Number := String} -> String;
        _                   -> throw(error)
    end
.

% -------------------------------------------------------
% The Map of numbers to strings. 
% -------------------------------------------------------
create_numbers_map()
->
    #{ 0    =>  "",
       1    =>  "one",
       2    =>  "two",
       3    =>  "three",
       4    =>  "four",
       5    =>  "five",
       6    =>  "six",
       7    =>  "seven",
       8    =>  "eight",
       9    =>  "nine",
       10   =>  "ten",
       11   =>  "eleven",
       12   =>  "twelve",
       13   =>  "thirteen",
       14   =>  "fourteen",
       15   =>  "fifteen",
       16   =>  "sixteen",
       17   =>  "seventeen",
       18   =>  "eighteen",
       19   =>  "nineteen",
       20   =>  "twenty",
       30   =>  "thirty",
       40   =>  "forty",
       50   =>  "fifty",
       60   =>  "sixty",
       70   =>  "seventy",
       80   =>  "eighty",
       90   =>  "ninety",
       1000 =>  "onethousand" }
.
