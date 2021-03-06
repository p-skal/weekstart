module Timestamp exposing (daysInMonth, format, formatSlashes, formatTime, getDay, getFullMonth, getMonth, getMonthFromNumber, getMonthNumber, getNextMonth, getNextMonthTime, getPrevMonth, getPrevMonthTime, toMonth, view)

import Derberos.Date.Calendar as Calendar
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, fail, succeed)
import Time exposing (Month(..), Weekday(..))



-- VIEW


view : Time.Zone -> Time.Posix -> Html msg
view timeZone timestamp =
    span [ class "date" ] [ text (format timeZone timestamp) ]



-- FORMAT


formatTime : Time.Zone -> Time.Posix -> String
formatTime zone time =
    let
        addZeros int =
            if int < 10 then
                "0"

            else
                ""

        dayNight =
            if Time.toHour zone time >= 12 then
                " PM"

            else
                " AM"
    in
    addZeros (Time.toHour zone time)
        ++ String.fromInt (Time.toHour zone time)
        ++ ":"
        ++ addZeros (Time.toMinute zone time)
        ++ String.fromInt (Time.toMinute zone time)
        ++ dayNight


{-| Format a timestamp as a String, like so:
"February 14, 2018"
For more complex date formatting scenarios, here's a nice package:
<https://package.elm-lang.org/packages/ryannhg/date-format/latest/>
-}
format : Time.Zone -> Time.Posix -> String
format zone time =
    let
        month =
            case Time.toMonth zone time of
                Jan ->
                    "January"

                Feb ->
                    "February"

                Mar ->
                    "March"

                Apr ->
                    "April"

                May ->
                    "May"

                Jun ->
                    "June"

                Jul ->
                    "July"

                Aug ->
                    "August"

                Sep ->
                    "September"

                Oct ->
                    "October"

                Nov ->
                    "November"

                Dec ->
                    "December"

        day =
            String.fromInt (Time.toDay zone time)

        year =
            String.fromInt (Time.toYear zone time)
    in
    month ++ " " ++ day ++ ", " ++ year


formatSlashes : Time.Zone -> Time.Posix -> String
formatSlashes zone time =
    let
        month =
            case Time.toMonth zone time of
                Jan ->
                    "1"

                Feb ->
                    "2"

                Mar ->
                    "3"

                Apr ->
                    "4"

                May ->
                    "5"

                Jun ->
                    "6"

                Jul ->
                    "7"

                Aug ->
                    "8"

                Sep ->
                    "9"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"

        day =
            String.fromInt (Time.toDay zone time)

        year =
            Time.toYear zone time
                |> String.fromInt
                |> String.dropLeft 2
    in
    day ++ "/" ++ month ++ "/" ++ year


toMonth : Time.Zone -> Time.Posix -> Int
toMonth zone time =
    getMonthNumber (Time.toMonth zone time)


getMonthNumber : Month -> Int
getMonthNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


getDay : Weekday -> String
getDay weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


getMonth : Month -> String
getMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


getMonthFromNumber : Int -> Month
getMonthFromNumber monthNumber =
    case monthNumber of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


getFullMonth : Month -> String
getFullMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


daysInMonth : Month -> Int -> Int
daysInMonth month year =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


isLeapYear : Int -> Bool
isLeapYear y =
    remainderBy 400 y == 0 || remainderBy 100 y /= 0 && remainderBy 4 y == 0



-- NEXT AND PREV MONTHS


getNextMonthTime : Time.Zone -> Time.Posix -> Time.Posix
getNextMonthTime timeZone timeNow =
    let
        last =
            Calendar.getLastDayOfMonth timeZone timeNow
    in
    Time.posixToMillis last
        + 24
        * 60
        * 60
        * 1000
        |> Time.millisToPosix
        |> Calendar.getFirstDayOfMonth timeZone


getNextMonth : Time.Zone -> Time.Posix -> ( Weekday, Month )
getNextMonth timeZone timeNow =
    let
        next =
            getNextMonthTime timeZone timeNow
    in
    ( Time.toWeekday timeZone next, Time.toMonth timeZone next )


getPrevMonthTime : Time.Zone -> Time.Posix -> Time.Posix
getPrevMonthTime timeZone timeNow =
    let
        first =
            Calendar.getFirstDayOfMonth timeZone timeNow
    in
    Time.posixToMillis first
        - 24
        * 60
        * 60
        * 1000
        |> Time.millisToPosix
        |> Calendar.getFirstDayOfMonth timeZone


getPrevMonth : Time.Zone -> Time.Posix -> ( Weekday, Month )
getPrevMonth timeZone timeNow =
    let
        prev =
            getPrevMonthTime timeZone timeNow
    in
    ( Time.toWeekday timeZone prev, Time.toMonth timeZone prev )
