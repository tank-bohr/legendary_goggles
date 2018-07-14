%% @doc Date calculations helper functions
%%
%%
%%
%% @end

-module(legendary_goggles_date_calc).

-export([
    is_valid_time/2,
    is_valid_date/2,
    shift_date/2,
    shift_date_by_unit/3,
    shift_date_to_weekday/3
]).

-type unit() :: day | week | month | year.
-type direction() :: -1 | 0 | 1.
-type weekday() :: 1..7.


-spec is_valid_time(Hour :: integer(), Minute :: integer()) -> boolean().
%% @doc
%% @end
is_valid_time(Hour, Minute) when Hour >= 0, Hour =< 23, Minute >= 0, Minute =< 59 ->
    true;
is_valid_time(_Hour, Minute) ->
    false.

-spec is_valid_date(Month :: integer(), Day :: integer()) -> boolean().
%% @doc
%% @end
is_valid_date(Month, Day) when Month =:= 2 andalso Day > 29 ->
    false;
is_valid_date(Month, Day) when Month < 1
                        orelse Month > 12
                        orelse Day < 1
                        orelse Day > 31 ->
    false;
is_valid_date(_Month, _Day) ->
    true.

-spec shift_date(Date :: legendary_goggles:date(), Shift :: integer() ) -> legendary_goggles:date().
%% @doc Shifts the date for a specified number of days
shift_date(Date, Shift) ->
    Days = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(Days + Shift).

-spec shift_date_by_unit(
    BaseDate :: legendary_goggles:date(),
    Unit :: unit(),
    Direction :: direction()
) -> legendary_goggles:date().
%% @doc Shifts date by single day, week, month or year
%% TODO: Captain
%% @end
shift_date_by_unit(BaseDate, _Unit, 0) ->
    BaseDate;
shift_date_by_unit(BaseDate, day, Direction) ->
    shift_date(BaseDate, Direction);
shift_date_by_unit(BaseDate, week, Direction) ->
    shift_date(BaseDate, 7 * Direction);
shift_date_by_unit(BaseDate, month, Direction) ->
    %% Naive implementaion
    shift_date(BaseDate, 30 * Direction);
shift_date_by_unit({Y, M, D}, year, Direction) ->
    Year = Y + Direction,
    case { calendar:is_leap_year(Year), M, D} of
        {false, 2, 29} -> {Year, 2, 28};
        _ -> {Year, M, D}
    end.

-spec shift_date_to_weekday(
    BaseDate :: legendary_goggles:date(),
    Weekday :: weekday(),
    Direction :: direction()
) -> legendary_goggles:date().
%% @doc Shifts date to the closest Monday
shift_date_to_weekday(BaseDate, Weekday, Direction) ->
    BaseWeekday = calendar:day_of_the_week(BaseDate),
    case Direction of
        1 ->
            WeekCompletion = 7 - BaseWeekday,
            shift_date(BaseDate, WeekCompletion + Weekday);
        0 ->
            shift_date(BaseDate, Weekday - BaseWeekday);
        -1 ->
            WeekCompletion = 7 - Weekday,
            shift_date(BaseDate, - BaseWeekday - WeekCompletion)
    end.
