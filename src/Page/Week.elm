module Page.Week exposing (Model, Msg, TaskType(..), init, subscriptions, toSession, update, view)

import Derberos.Date.Calendar as Calendar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Page
import Session exposing (Session)
import Task
import Time
import Timestamp



-- MODEL


type TaskType
    = Normal
    | Important
    | Upcoming
    | Past


type alias Task =
    { day : Time.Weekday
    , details : String
    , time :
        { hour : Int
        , min : Int
        }
    , taskType : TaskType
    }


type alias Model =
    { session : Session
    , magic : TaskForm
    , timeZone : Time.Zone
    , timeNow : Time.Posix
    , tasks : List Task
    , modal : Bool
    }


type alias Modal =
    { show : Bool
    , title : String
    , content : List (Html Msg)
    }


type alias TaskForm =
    { day : Time.Weekday
    , details : String
    , time : Time.Posix
    , timeHour : Int
    , timeMin : Int
    }


type alias Day =
    { day : Time.Weekday
    , tasks : List Task
    , newTask : String
    , newTaskTime : Time.Posix
    , newTaskType : TaskType
    }



-- INIT


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , magic = TaskForm Time.Mon "" (Time.millisToPosix 0) 0 0
      , timeZone = Time.customZone 660 []
      , timeNow = Time.millisToPosix 0
      , tasks =
            [ Task Time.Mon "Get up for uni" { hour = 8, min = 0 } Important
            , Task Time.Tue "Get up for uni" { hour = 8, min = 0 } Important
            ]
      , modal = False
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        , Task.perform GotTimeNow Time.now
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    let
        dates =
            Calendar.getCurrentWeekDates model.timeZone model.timeNow
    in
    { title = "Week"
    , content =
        main_ []
            [ section [ class "top" ]
                [ div [ class "wrapper" ]
                    [ viewPageHeader model.timeZone model.timeNow
                    , div [ class "grid week-grid" ] <|
                        {- List.map (viewWeekday model.timeZone model.timeNow) model.tasks
                           [ viewWeekday model.tasks Time.Mon model.timeZone model.timeNow model.magic
                           , viewWeekday model.tasks Time.Tue model.timeZone model.timeNow model.magic
                           , viewWeekday model.tasks Time.Wed model.timeZone model.timeNow model.magic
                           , viewWeekday model.tasks Time.Thu model.timeZone model.timeNow model.magic
                           , viewWeekday model.tasks Time.Fri model.timeZone model.timeNow model.magic
                           , viewWeekday model.tasks Time.Sat model.timeZone model.timeNow model.magic
                           , viewWeekday model.tasks Time.Sun model.timeZone model.timeNow model.magic
                           , div [ class "week-day" ] []
                           ]
                        -}
                        List.map (viewWeekday model.timeZone model.timeNow model.tasks model.magic) dates
                    ]
                ]
            , viewModal model.modal "Enter a time for your task..." [ input [ type_ "number", placeholder "00", Html.Attributes.max "24", onInput EnteredTimeHour ] [], text ":", input [ type_ "number", Html.Attributes.max "59", placeholder "00", onInput EnteredTimeMin ] [], button [ class "btn btn-primary", onClick AddTask ] [ text "Enter" ] ]
            ]
    }


viewPageHeader : Time.Zone -> Time.Posix -> Html Msg
viewPageHeader timeZone timeNow =
    let
        firstSixMonths =
            [ Time.Jan
            , Time.Feb
            , Time.Mar
            , Time.Apr
            , Time.May
            , Time.Jun
            ]

        lastSixMonths =
            [ Time.Jul
            , Time.Aug
            , Time.Sep
            , Time.Oct
            , Time.Nov
            , Time.Dec
            ]

        isMonth current =
            if Time.toMonth timeZone timeNow == current then
                class "today active"

            else
                class ""

        viewMonth month =
            li [ isMonth month ] [ button [] [ text (Timestamp.getMonth month) ] ]
    in
    div [ class "page-header" ]
        [ ul [ class "nav nav-left" ] <|
            List.map viewMonth firstSixMonths
        , div [ class "page-title" ]
            [ h1 [] [ text "The Weekly Brief" ]
            , small [] [ b [] [ Timestamp.view timeZone timeNow ] ]
            ]
        , ul [ class "nav nav-right" ] <|
            List.map viewMonth lastSixMonths
        , ul [ class "nav" ]
            [ li [ class "nav-trigger" ]
                [ button [] [ i [ style "marginRight" ".5rem", class "fas fa-bars" ] [], text "Months" ]
                ]
            ]
        ]


viewModal modal title content =
    let
        _ =
            Debug.log "MAMAMA" modal
    in
    case modal of
        True ->
            div [ class "modal-container" ]
                [ div [ class "modal" ] <|
                    [ h2 []
                        [ text title
                        ]
                    ]
                        ++ content
                ]

        False ->
            text ""


viewWeekday : Time.Zone -> Time.Posix -> List Task -> TaskForm -> Time.Posix -> Html Msg
viewWeekday timeZone timeNow tasks form dateTime =
    let
        date =
            Timestamp.formatSlashes timeZone dateTime

        isToday =
            if Time.toWeekday timeZone timeNow == Time.toWeekday timeZone dateTime then
                span [ class "tag" ] [ text "Today" ]

            else
                text ""
    in
    div [ class "grid-day", classList [ ( "today", Time.toDay timeZone timeNow == Time.toDay timeZone dateTime ) ] ] <|
        [ span [ class "day" ]
            [ text (Timestamp.getDay (Time.toWeekday timeZone dateTime))
            , small [ class "date" ] [ text date ]
            , isToday
            ]
        , inputgroup "Add a task"
            form
            (EnteredTask (Time.toWeekday timeZone dateTime))
            [ button [ class "input-icon", onClick SelectTime ] [ i [ class "far fa-clock" ] [] ]
            ]
        , List.filter (\task -> task.day == Time.toWeekday timeZone dateTime) tasks
            |> viewTaskList timeZone timeNow dateTime
        ]


viewTaskList : Time.Zone -> Time.Posix -> Time.Posix -> List Task -> Html msg
viewTaskList timeZone timeNow dateTime tasks =
    tasks
        |> List.sortBy (\t -> t.time.min)
        |> List.sortBy (\t -> t.time.hour)
        |> List.map (viewTask timeZone timeNow dateTime)
        |> ul [ class "task-list" ]


viewTask timeZone timeNow dateTime task =
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

        typeee =
            if Time.posixToMillis timeNow > Time.posixToMillis dateTime + 1000 * 60 * 60 * task.time.hour + 1000 * 60 * task.time.min then
                class "past"

            else
                class ""

        isNow =
            case task.taskType of
                Upcoming ->
                    text " • now"

                _ ->
                    text ""

        addZeros int =
            if int < 10 then
                "0"

            else
                ""

        dayNight =
            if task.time.hour >= 12 then
                " PM"

            else
                " AM"

        taskTime =
            {- String.fromInt (Time.toHour timeZone task.time)
               ++ ":"
               ++ String.fromInt (Time.toMinute timeZone task.time)
               ++ (if Time.toHour timeZone task.time < 12 then
                       "AM"

                   else
                       "PM"
                  )
            -}
            addZeros task.time.hour
                ++ String.fromInt task.time.hour
                ++ ":"
                ++ addZeros task.time.min
                ++ String.fromInt task.time.min
                ++ dayNight
    in
    li [ class "task", taskType, typeee ]
        [ span [ class "time" ] [ text taskTime, isNow ]
        , text task.details
        ]


inputgroup : String -> TaskForm -> (String -> msg) -> List (Html msg) -> Html msg
inputgroup title form inputMsg content =
    node "inputgroup"
        [ classList [ ( "active", form.details /= "" ) ]
        ]
    <|
        [ label [] [ text title ]
        , input
            [ class "form-control input"
            , placeholder title
            , onInput inputMsg
            , value form.details
            ]
            []
        ]
            ++ content



-- UPDATE


type Msg
    = GotTimeZone Time.Zone
    | GotTimeNow Time.Posix
    | EnteredTask Time.Weekday String
    | EnteredTimeHour String
    | EnteredTimeMin String
    | SelectTime
    | AddTask
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredTask day details ->
            let
                m =
                    model.magic
            in
            ( { model | magic = { m | day = day, details = details } }, Cmd.none )

        EnteredTimeHour time ->
            let
                m =
                    model.magic

                newTime =
                    if String.length time > 2 then
                        String.slice 0 1 time
                            |> String.toInt
                            |> Maybe.withDefault 0

                    else
                        time
                            |> String.toInt
                            |> Maybe.withDefault 0
            in
            ( { model | magic = { m | timeHour = newTime } }, Cmd.none )

        EnteredTimeMin time ->
            let
                m =
                    model.magic

                newTime =
                    if String.length time > 2 then
                        String.slice 0 1 time
                            |> String.toInt
                            |> Maybe.withDefault 0

                    else
                        time
                            |> String.toInt
                            |> Maybe.withDefault 0
            in
            ( { model | magic = { m | timeMin = newTime } }, Cmd.none )

        SelectTime ->
            let
                m =
                    model.modal
            in
            if model.modal == False then
                ( { model | modal = True }, Cmd.none )

            else
                ( { model | modal = False }, Cmd.none )

        AddTask ->
            case model.magic.details of
                "" ->
                    ( { model | modal = False }, Cmd.none )

                _ ->
                    ( { model | tasks = model.tasks ++ [ Task model.magic.day model.magic.details { hour = model.magic.timeHour, min = model.magic.timeMin } Normal ], magic = TaskForm model.magic.day "" model.timeNow 0 0, modal = False }, Cmd.none )

        GotTimeZone tz ->
            ( { model | timeZone = Time.customZone 660 [] }, Cmd.none )

        GotTimeNow time ->
            ( { model | timeNow = time }, Cmd.none )

        Tick newTime ->
            ( { model | timeNow = newTime }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
