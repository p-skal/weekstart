module Timestamp exposing (format, formatSlashes, getMonth,getDay, view)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, fail, succeed)
import Time exposing (Month(..), Weekday(..))



-- VIEW


view : Time.Zone -> Time.Posix -> Html msg
view timeZone timestamp =
    span [ class "date" ] [ text (format timeZone timestamp) ]



-- FORMAT


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
