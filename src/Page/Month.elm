module Page.Month exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Derberos.Date.Calendar as Calendar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Session exposing (Session)
import Task
import Time
import Timestamp



-- MODEL


type alias Model =
    { session : Session
    , timeNow : Time.Posix
    , timeZone : Time.Zone
    , viewMonth : Time.Month
    , viewMonthDayPosixTimes : List Time.Posix
    , viewYear : Int
    }


type Date
    = Date Int Time.Month Int



-- INIT


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , timeNow = Time.millisToPosix 0
      , timeZone = Time.customZone 660 []
      , viewMonth = Time.Jan
      , viewMonthDayPosixTimes = []
      , viewYear = 1970
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
        currentDate =
            Time.toDay model.timeZone model.timeNow

        firstDayMonth =
            case List.head model.viewMonthDayPosixTimes of
                Just list ->
                    list

                _ ->
                    Time.millisToPosix 0

        firstDayMonthNumber =
            Time.toDay model.timeZone firstDayMonth

        {- first =
               Calendar.getFirstDayOfMonth model.timeZone model.timeNow

           last =
               Calendar.getLastDayOfMonth model.timeZone model.timeNow

           prev =
               Time.posixToMillis first
                   - 24
                   * 60
                   * 60
                   * 1000
                   |> Time.millisToPosix
                   |> Calendar.getFirstDayOfMonth model.timeZone

           next =
               Time.posixToMillis last
                   + 24
                   * 60
                   * 60
                   * 1000
                   |> Time.millisToPosix
                   |> Calendar.getFirstDayOfMonth model.timeZone
        -}
        monthPrevNext preDayPosix =
            (String.fromInt <|
                Time.toHour model.timeZone <|
                    preDayPosix
            )
                ++ ":"
                ++ (String.fromInt <|
                        Time.toMinute model.timeZone <|
                            preDayPosix
                   )
                ++ " "
                ++ (Timestamp.getDay <|
                        Time.toWeekday model.timeZone <|
                            preDayPosix
                   )
                ++ " "
                ++ (String.fromInt <|
                        Time.toDay model.timeZone <|
                            preDayPosix
                   )
                ++ " "
                ++ (Timestamp.getFullMonth <|
                        Time.toMonth model.timeZone <|
                            preDayPosix
                   )
                ++ " "
                ++ (String.fromInt <|
                        Time.toYear model.timeZone <|
                            preDayPosix
                   )
    in
    { title = "Calendar"
    , content =
        main_ []
            [ section [ class "top" ]
                [ div [ class "wrapper" ]
                    [ viewPageHeader model

                    --, h1 [] [ text "NEXT: ", text (monthPrevNext (Timestamp.getNextMonthTime model.timeZone firstDayMonth)) ]
                    , div [ class "grid month-grid" ] <|
                        [ div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Sunday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Monday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Tuesday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Wednesday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Thursday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Friday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Saturday" ] ]
                        ]
                            ++ List.map (\p -> div [ class "" ] [ text "test" ])
                                (List.range 1 (dayOfWeek (Time.toWeekday model.timeZone firstDayMonth)))
                            --++ viewPrevMonthDays model model.viewMonth model.viewYear firstDayMonth
                            ++ viewMonthDays model.timeZone model.timeNow model.viewMonth model.viewYear model.viewMonthDayPosixTimes
                    , br [] []
                    , button [ class "btn btn-primary", onClick <| SwitchedYear (model.viewYear - 1) ] [ text "Prev Year" ]
                    , button [ class "btn  btn-primary", onClick <| SwitchedYear (model.viewYear + 1) ] [ text "Next Year" ]
                    , button [ onClick SwitchedPrevMonth ] [ text "Previous Month" ]
                    , button [ onClick SwitchedNextMonth ] [ text "Next Month" ]
                    ]
                ]
            ]
    }


viewMonthDays timeZone timeNow viewMonth viewYear viewMonthDayPosixTimes =
    List.map
        (\a ->
            div [ class "grid-day", classList [ ( "today", Time.toDay timeZone timeNow == Time.toDay timeZone a && Time.toYear timeZone timeNow == viewYear && Time.toMonth timeZone timeNow == viewMonth ) ] ]
                [ span [ class "day" ] [ text <| String.fromInt (Time.toDay timeZone a) ]

                --    , span [ class "tag" ] [ text <| Timestamp.getDay <| Time.toWeekday timeZone a ]
                , node "inputgroup"
                    []
                    [ label [] [ text "Enter a task..." ]
                    , input
                        [ class "form-control input"
                        , placeholder "Enter task..."
                        ]
                        []
                    ]
                ]
        )
        viewMonthDayPosixTimes


viewPrevMonthDays : Model -> Time.Month -> Int -> Time.Posix -> List (Html msg)
viewPrevMonthDays model month year firstDayNextMonth =
    let
        preMonth =
            Timestamp.getMonthFromNumber <| Timestamp.getMonthNumber month - 1

        preMonthNumberOfDays =
            Timestamp.daysInMonth preMonth year

        preMonthDayPosixTimes =
            Calendar.getCurrentMonthDates model.timeZone model.timeNow

        firstNextInt =
            Time.toDay model.timeZone firstDayNextMonth
    in
    List.map
        (\d ->
            div [ class "grid-day" ] [ text (String.fromInt (Time.toDay model.timeZone d)) ]
        )
        (List.drop (preMonthNumberOfDays - firstNextInt)
            preMonthDayPosixTimes
        )


dayOfWeek : Time.Weekday -> Int
dayOfWeek weekday =
    case weekday of
        Time.Mon ->
            1

        Time.Tue ->
            2

        Time.Wed ->
            3

        Time.Thu ->
            4

        Time.Fri ->
            5

        Time.Sat ->
            6

        Time.Sun ->
            7


viewPageHeader : Model -> Html Msg
viewPageHeader model =
    let
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
            if Time.toMonth model.timeZone model.timeNow == current && model.viewMonth == current then
                class "active today"

            else if Time.toMonth model.timeZone model.timeNow == current then
                class "today"

            else if model.viewMonth == current then
                class "active"

            else
                class ""

        viewMonth m =
            li [ isMonth m ] [ button [] [ text (Timestamp.getMonth m) ] ]
    in
    div [ class "page-header" ]
        [ ul [ class "nav nav-left" ] <|
            List.map viewMonth (monthsOfTheYear |> List.reverse |> List.drop 6 |> List.reverse)
        , div [ class "page-title" ]
            [ h1 [] [ text "The Monthly Brief" ]
            , small [] [ b [] [ text <| Timestamp.getFullMonth model.viewMonth ++ ", " ++ String.fromInt model.viewYear ] ]
            ]
        , ul [ class "nav nav-right" ] <|
            List.map viewMonth (List.drop 6 monthsOfTheYear)
        , ul [ class "nav" ]
            [ li [ class "nav-trigger" ]
                [ button [] [ i [ style "marginRight" ".5rem", class "fas fa-bars" ] [], text "Months" ]
                ]
            ]
        ]



-- UPDATE


type Msg
    = NoOp
    | GotTimeZone Time.Zone
    | GotTimeNow Time.Posix
    | SwitchedNextMonth
    | SwitchedPrevMonth
    | SwitchedYear Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTimeZone tz ->
            ( { model | timeZone = Time.customZone 660 [] }, Cmd.none )

        GotTimeNow time ->
            ( { model
                | timeNow = time
                , viewMonth = Time.toMonth model.timeZone time
                , viewMonthDayPosixTimes = Calendar.getCurrentMonthDates model.timeZone time
                , viewYear = Time.toYear model.timeZone time
              }
            , Cmd.none
            )

        SwitchedNextMonth ->
            let
                {- switchedToMonth =
                       Timestamp.getMonthNumber m

                   switchedFromMonth =
                       Timestamp.getMonthNumber model.viewMonth

                   newMonthTime =
                       if switchedToMonth > switchedFromMonth then
                           Timestamp.getNextMonthTime model.timeZone model.timeNow

                       else if switchedToMonth < switchedFromMonth then
                           Timestamp.getPrevMonthTime model.timeZone model.timeNow

                       else
                           Calendar.getFirstDayOfMonth model.timeZone model.timeNow
                -}
                newMonthTime =
                    case List.head model.viewMonthDayPosixTimes of
                        Just a ->
                            Timestamp.getNextMonthTime model.timeZone a

                        _ ->
                            model.timeNow

                newMonth =
                    Timestamp.getMonthNumber model.viewMonth
                        + 1
                        |> Timestamp.getMonthFromNumber

                _ =
                    Debug.log "NEXT MONTH" newMonth

                monthPrevNext preDayPosix =
                    (String.fromInt <|
                        Time.toHour model.timeZone <|
                            preDayPosix
                    )
                        ++ ":"
                        ++ (String.fromInt <|
                                Time.toMinute model.timeZone <|
                                    preDayPosix
                           )
                        ++ " "
                        ++ (Timestamp.getDay <|
                                Time.toWeekday model.timeZone <|
                                    preDayPosix
                           )
                        ++ " "
                        ++ (String.fromInt <|
                                Time.toDay model.timeZone <|
                                    preDayPosix
                           )
                        ++ " "
                        ++ (Timestamp.getFullMonth <|
                                Time.toMonth model.timeZone <|
                                    preDayPosix
                           )
                        ++ " "
                        ++ (String.fromInt <|
                                Time.toYear model.timeZone <|
                                    preDayPosix
                           )

                _ =
                    Debug.log "NEW MONTH" (monthPrevNext newMonthTime)
            in
            ( { model | viewMonth = newMonth, viewMonthDayPosixTimes = Calendar.getCurrentMonthDates model.timeZone newMonthTime }, Cmd.none )

        SwitchedPrevMonth ->
            let
                newMonthTime =
                    case List.head model.viewMonthDayPosixTimes of
                        Just a ->
                            Timestamp.getPrevMonthTime model.timeZone a

                        _ ->
                            model.timeNow

                newMonth =
                    Timestamp.getMonthNumber model.viewMonth
                        - 1
                        |> Timestamp.getMonthFromNumber
            in
            ( { model | viewMonth = newMonth, viewMonthDayPosixTimes = Calendar.getCurrentMonthDates model.timeZone newMonthTime }, Cmd.none )

        SwitchedYear y ->
            ( { model | viewYear = y }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
