module WeekTask exposing (Form, Type(..), WeekTask, viewList, viewOne)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Timestamp


type alias Form =
    { day : Time.Weekday
    , details : String
    , time : Time.Posix
    , timeHour : Int
    , timeMin : Int
    }


type Type
    = Normal
    | Important
    | Upcoming
    | Past
    | Tracker


type alias WeekTask =
    { day : Time.Weekday
    , details : String
    , time : Time.Posix
    , taskType : Type
    }


viewList : Time.Zone -> Time.Posix -> Time.Posix -> (WeekTask -> String -> msg) -> List WeekTask -> Html msg
viewList timeZone timeNow dateTime editingMsg tasks =
    let
        tracker =
            if Time.toDay timeZone timeNow == Time.toDay timeZone dateTime && tasks /= [] then
                [ WeekTask (Time.toWeekday timeZone timeNow) "" timeNow Tracker ]

            else
                []
    in
    (tasks ++ tracker)
        |> List.sortBy (\t -> Time.posixToMillis t.time)
        |> List.map (viewOne timeZone timeNow dateTime editingMsg)
        |> ul [ class "task-list" ]


viewOne : Time.Zone -> Time.Posix -> Time.Posix -> (WeekTask -> String -> msg) -> WeekTask -> Html msg
viewOne timeZone timeNow dateTime editingMsg task =
    let
        taskType =
            case task.taskType of
                Normal ->
                    class ""

                Important ->
                    class "important"

                Upcoming ->
                    class "important"

                Past ->
                    class "past"

                Tracker ->
                    class "time-now"

        typeee =
            if Time.posixToMillis timeNow > Time.posixToMillis task.time then
                class "past"

            else if Time.posixToMillis task.time > Time.posixToMillis timeNow && Time.posixToMillis task.time < Time.posixToMillis timeNow + 1000 * 60 * 60 then
                class "upcoming"

            else
                class ""

        isNow =
            if typeee == class "upcoming" then
                span [ style "textTransform" "lowercase" ] [ text " • next" ]

            else
                text ""
    in
    li [ class "task", taskType, typeee, title (Timestamp.formatTime timeZone task.time) ]
        [ if task.taskType == Tracker then
            text ""

          else
            span [ class "time" ] [ text (Timestamp.formatTime timeZone task.time), isNow ]
        , input [ type_ "text", onInput (editingMsg task), value task.details ] [ text task.details ]
        ]
