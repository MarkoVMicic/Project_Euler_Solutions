% Problem link: https://projecteuler.net/problem=19


% Problem Statement:
% You are given the following information, but you may prefer to do some 
% research for yourself.

% 1 Jan 1900 was a Monday.
% Thirty days has September,
% April, June and November.
% All the rest have thirty-one,
% Saving February alone,
% Which has twenty-eight, rain or shine.
% And on leap years, twenty-nine.
% A leap year occurs on any year evenly divisible by 4, but not on a century 
% unless it is divisible by 400.
% How many Sundays fell on the first of the month during the twentieth century 
% (1 Jan 1901 to 31 Dec 2000)?

-module(problem19).
-export([main/2]).  

% NOTE: The Reference_Date in this program is hard-coded to be Monday, the 1st 
%       of January 1990. This is hard-coded in the first line of main, and the 
%       function find_count_from_day/2 expects the Reference_Date to be 
%       earlier than Count_From. 

main( {Count_From_Year, Count_From_Month, Count_From_Date} = Count_From,
      {_Count_To_Year,   _Count_To_Month,   _Count_To_Date  } = Count_To  )
->
    Reference_Date = { 1900, 1, 1, 1 },
    Count_From_Day = find_count_from_day( Reference_Date, Count_From),
    count_sundays_on_first_of_month( { Count_From_Year, 
                                       Count_From_Month, 
                                       Count_From_Date,
                                       Count_From_Day },
                                       Count_To )
.

% ----------------------------------------------------------------------------
% Advance from the reference date to the count_from date, to find out what day 
% the count_from date begins on. 
% ----------------------------------------------------------------------------
find_count_from_day( {Year, Month, Date, Day}, 
                     {Year, Month, Date} )
->
    Day
;
find_count_from_day( Reference_Date,
                     Count_From )
->
    find_count_from_day( add_one_day(Reference_Date), 
                         Count_From )
.

% ------------------------------------------------
% Count Sundays that fall on the first of a month. 
% ------------------------------------------------
count_sundays_on_first_of_month( Count_From, Count_To )
->
    count_sundays_on_first_of_month( Count_From, Count_To, 0 )
.
count_sundays_on_first_of_month( {Year, Month, Date, _Day},
                                 {Year, Month, Date},
                                 Acc )
->
    Acc
;
count_sundays_on_first_of_month( {_Year, _Month, 1, 0} = Count_From, 
                                 Count_To, 
                                 Acc )
->
    count_sundays_on_first_of_month( add_one_day(Count_From), 
                                     Count_To,
                                     Acc + 1)
;
count_sundays_on_first_of_month( Count_From, 
                                 Count_To,
                                 Acc )
->
    count_sundays_on_first_of_month( add_one_day(Count_From),
                                     Count_To,
                                     Acc )
.
% ------------------------------------------------
% Add one day to a {Year, Month, Date, Day} tuple.
% ------------------------------------------------

% ------------------------------------
% End of: Jan, Mar, May, Jul, Aug, Oct
%         These months have 31 days.
% ------------------------------------
add_one_day( {Year, Month, 31, Day} ) when Month == 1 orelse % Jan
                                           Month == 3 orelse % Mar
                                           Month == 5 orelse % May
                                           Month == 7 orelse % Jul
                                           Month == 8 orelse % Aug
                                           Month == 10       % Oct
->
    { Year, Month + 1, 1, (Day + 1) rem 7 }
;
% ----------------------------------
% End of: Apr, Jun, Sep, Nov
%         These months have 30 days.
% ----------------------------------
add_one_day( {Year, Month, 30, Day} ) when Month == 4 orelse % Apr
                                           Month == 6 orelse % Jun
                                           Month == 9 orelse % Sep
                                           Month == 11       % Nov
->
    { Year, Month + 1, 1, (Day + 1) rem 7 }
;
% ---------------------
% End of Feb: Leap Year
% ---------------------
add_one_day( {Year, 2, 28, Day} ) when Year rem 4 == 0 andalso
                                       ( Year rem 100 =/= 0 orelse 
                                         Year rem 400 == 0 )
->
    { Year, 2, 29, (Day + 1) rem 7 }
;
add_one_day( {Year, 2, 29, Day} ) when Year rem 4 == 0 andalso
                                       ( Year rem 100 =/= 0 orelse 
                                         Year rem 400 == 0 )
->
    { Year, 3, 1, (Day + 1) rem 7 }
;
% -------------------------
% End of Feb: Non-Leap Year
% -------------------------
add_one_day( {Year, 2, 28, Day} )
->
    { Year, 3, 1, (Day + 1) rem 7 }
;
% ----------------------
% End of Year
% December has 31 days.
% ----------------------
add_one_day( {Year, 12, 31, Day} ) 
->
    { Year + 1, 1, 1, (Day + 1) rem 7 }
;
% -------------------------
% In the middle of a month.
% -------------------------
add_one_day( {Year, Month, Date, Day } )
->
    { Year, Month, Date + 1, (Day + 1) rem 7 }
.