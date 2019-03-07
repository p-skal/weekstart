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
            Calendar.getFirstDayOfMonth model.timeZone model.timeNow
                |> Time.toWeekday model.timeZone

        _ =
            Debug.log "1st day month"
                (Calendar.getCurrentWeekDates model.timeZone model.timeNow)
    in
    { title = "Calendar"
    , content =
        main_ []
            [ section [ class "top" ]
                [ div [ class "wrapper" ]
                    [ viewPageHeader model
                    , div [ class "grid month-grid" ] <|
                        [ div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Sunday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Monday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Tuesday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Wednesday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Thursday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Friday" ] ]
                        , div [ class "grid-day grid-header" ] [ span [ class "day" ] [ text "Saturday" ] ]
                        ]
                            ++ viewPrevMonthDays model.viewMonth model.viewYear firstDayMonth
                            ++ List.map
                                (\a ->
                                    div [ class "grid-day", classList [ ( "today", Time.toDay model.timeZone model.timeNow == a && Time.toYear model.timeZone model.timeNow == model.viewYear && Time.toMonth model.timeZone model.timeNow == model.viewMonth ) ] ]
                                        [ span [ class "day" ] [ text (String.fromInt a) ]
                                        , span [ class "tag" ] [ text <| Timestamp.getDay <| Time.toWeekday model.timeZone model.timeNow ]
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
                                (List.range 1 (Timestamp.daysInMonth model.viewMonth model.viewYear))
                    , br [] []
                    , button [ class "btn btn-primary", onClick <| SwitchedYear (model.viewYear - 1) ] [ text "Prev Year" ]
                    , button [ class "btn  btn-primary", onClick <| SwitchedYear (model.viewYear + 1) ] [ text "Next Year" ]
                    ]
                ]
            ]
    }


viewPrevMonthDays : Time.Month -> Int -> Time.Weekday -> List (Html msg)
viewPrevMonthDays month year firstDayNextMonth =
    let
        preMonth =
            Timestamp.getMonthFromNumber <| Timestamp.getMonthNumber month - 1
    in
    List.map
        (\d ->
            div [ class "grid-day" ] [ text (String.fromInt d) ]
        )
        (List.range
            (Timestamp.daysInMonth preMonth year - dayOfWeek firstDayNextMonth + 1)
            (Timestamp.daysInMonth preMonth year)
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
            li [ isMonth m ] [ button [ onClick (SwitchedMonth m) ] [ text (Timestamp.getMonth m) ] ]
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
    | SwitchedMonth Time.Month
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
                , viewYear = Time.toYear model.timeZone time
              }
            , Cmd.none
            )

        SwitchedMonth m ->
            ( { model | viewMonth = m }, Cmd.none )

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
