module Page.Week exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Derberos.Date.Calendar as Calendar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Page
import Session exposing (Session)
import Task
import Time
import Timestamp
import WeekTask exposing (WeekTask)



-- MODEL


type alias Model =
    { session : Session
    , magic : WeekTask.Form
    , timeZone : Time.Zone
    , timeNow : Time.Posix
    , timeOfDaySelected : Time.Posix
    , viewWeekTimes : List Time.Posix
    , tasks : List WeekTask
    , modal : Bool
    }


type alias Modal =
    { show : Bool
    , title : String
    , content : List (Html Msg)
    }


type alias Day =
    { day : Time.Weekday
    , tasks : List WeekTask
    , newTask : String
    , newTaskTime : Time.Posix
    , newTaskType : WeekTask.Type
    }



-- INIT


init : Session -> ( Model, Cmd Msg )
init session =
    let
        initTime =
            Time.millisToPosix 0

        ausZone =
            Time.customZone 660 []
    in
    ( { session = session
      , magic = WeekTask.Form Time.Mon "" initTime 0 0
      , timeZone = ausZone
      , timeNow = initTime
      , timeOfDaySelected = initTime
      , viewWeekTimes = []
      , tasks =
            [ WeekTask Time.Mon "Get up for uni" initTime WeekTask.Important
            , WeekTask Time.Tue "Get up for uni" initTime WeekTask.Important
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
    { title = "Week"
    , content =
        main_ []
            [ section [ class "top" ]
                [ div [ class "wrapper" ]
                    [ viewPageHeader model.timeZone model.timeNow (Maybe.withDefault model.timeNow <| List.head model.viewWeekTimes)
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
                        List.map (viewWeekday model.timeZone model.timeNow model.tasks model.magic) model.viewWeekTimes
                    , br [] []
                    , button [ onClick SwitchedPrevWeek ] [ text "Previous Week" ]
                    , button [ onClick SwitchedNextWeek ] [ text "Next Week" ]
                    ]
                ]
            , viewModal model.modal "Enter a time for your task..." [ input [ type_ "number", placeholder "00", Html.Attributes.max "24", onInput EnteredTimeHour, autofocus True ] [], text ":", input [ type_ "number", Html.Attributes.max "59", placeholder "00", onInput EnteredTimeMin ] [], button [ class "btn btn-primary", onClick AddTask ] [ text "Enter" ] ]
            ]
    }


viewPageHeader : Time.Zone -> Time.Posix -> Time.Posix -> Html Msg
viewPageHeader timeZone timeNow viewWeekTime =
    let
        _ =
            Debug.log "ALALLALA" (Time.toMonth timeZone viewWeekTime)

        monthsOfTheYear =
            [ Time.Jan
            , Time.Feb
            , Time.Mar
            , Time.Apr
            , Time.May
            , Time.Jun
            , Time.Jul
            , Time.Aug
            , Time.Sep
            , Time.Oct
            , Time.Nov
            , Time.Dec
            ]

        isMonth current =
            if Time.toMonth timeZone viewWeekTime == current then
                class "active"

            else if Time.toMonth timeZone timeNow == current then
                class "today active"

            else
                class ""

        viewMonth month =
            li [ isMonth month ] [ button [] [ text (Timestamp.getMonth month) ] ]
    in
    div [ class "page-header" ]
        [ ul [ class "nav nav-left" ] <|
            List.map viewMonth (List.take 6 monthsOfTheYear)
        , div [ class "page-title" ]
            [ h1 [] [ text "The Weekly Brief" ]
            , small [] [ b [] [ text "Welcome back, Peter 👋 ", Timestamp.view timeZone timeNow ] ]
            ]
        , ul [ class "nav nav-right" ] <|
            List.map viewMonth (List.reverse <| List.take 6 <| List.reverse monthsOfTheYear)
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


viewWeekday : Time.Zone -> Time.Posix -> List WeekTask -> WeekTask.Form -> Time.Posix -> Html Msg
viewWeekday timeZone timeNow tasks form dateTime =
    let
        date =
            Timestamp.formatSlashes timeZone dateTime

        isToday =
            if Time.toDay timeZone timeNow == Time.toDay timeZone dateTime && Time.toMonth timeZone timeNow == Time.toMonth timeZone dateTime && Time.toYear timeZone timeNow == Time.toYear timeZone dateTime then
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
            [ button [ class "input-icon", onClick (SelectTime dateTime) ] [ i [ class "far fa-clock" ] [] ]
            ]
        , tasks
            |> List.filter (\task -> Time.toDay timeZone task.time == Time.toDay timeZone dateTime)
            |> List.filter (\task -> Time.toMonth timeZone task.time == Time.toMonth timeZone dateTime)
            |> List.filter (\task -> Time.toYear timeZone task.time == Time.toYear timeZone dateTime)
            |> WeekTask.viewList timeZone timeNow dateTime EditingTask
        ]


inputgroup : String -> WeekTask.Form -> (String -> msg) -> List (Html msg) -> Html msg
inputgroup title form inputMsg content =
    node "inputgroup"
        [ classList [ ( "active", form.details /= "" ) ]
        ]
    <|
        [ label [] [ text title ]
        , input
            [ class "form-control input"
            , placeholder (title ++ "...")
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
    | SelectTime Time.Posix
    | AddTask
    | Tick Time.Posix
    | EditingTask WeekTask String
    | SwitchedNextWeek
    | SwitchedPrevWeek


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

        SelectTime dateTime ->
            let
                m =
                    model.modal
            in
            if model.modal == False then
                ( { model | modal = True, timeOfDaySelected = dateTime }, Cmd.none )

            else
                ( { model | modal = False, timeOfDaySelected = model.timeNow }, Cmd.none )

        AddTask ->
            let
                taskTime =
                    Time.posixToMillis model.timeOfDaySelected
                        + 1000
                        * 60
                        * 60
                        * model.magic.timeHour
                        + 1000
                        * 60
                        * model.magic.timeMin
                        |> Time.millisToPosix
            in
            case model.magic.details of
                "" ->
                    ( { model | modal = False }, Cmd.none )

                _ ->
                    ( { model | tasks = model.tasks ++ [ WeekTask model.magic.day model.magic.details taskTime WeekTask.Normal ], magic = WeekTask.Form model.magic.day "" model.timeNow 0 0, modal = False }, Cmd.none )

        GotTimeZone tz ->
            ( { model | timeZone = Time.customZone 660 [] }, Cmd.none )

        GotTimeNow time ->
            ( { model
                | timeNow = time
                , viewWeekTimes = Calendar.getCurrentWeekDates model.timeZone time
              }
            , Cmd.none
            )

        Tick newTime ->
            ( { model | timeNow = newTime }, Cmd.none )

        EditingTask task newTaskDetails ->
            let
                taskList =
                    List.filter (\a -> a /= task) model.tasks

                newTasks =
                    taskList ++ [ { task | details = newTaskDetails } ]
            in
            ( { model | tasks = newTasks }, Cmd.none )

        SwitchedNextWeek ->
            let
                nextWeekTime =
                    Time.millisToPosix <| Time.posixToMillis (Maybe.withDefault model.timeNow <| List.head model.viewWeekTimes) + 168 * 60 * 60 * 1000
            in
            ( { model | viewWeekTimes = Calendar.getCurrentWeekDates model.timeZone nextWeekTime }, Cmd.none )

        SwitchedPrevWeek ->
            let
                nextWeekTime =
                    Time.millisToPosix <| Time.posixToMillis (Maybe.withDefault model.timeNow <| List.head model.viewWeekTimes) - 168 * 60 * 60 * 1000
            in
            ( { model | viewWeekTimes = Calendar.getCurrentWeekDates model.timeZone nextWeekTime }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
