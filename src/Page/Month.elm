module Page.Month exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

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
    }



-- INIT


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , timeNow = Time.millisToPosix 0
      , timeZone = Time.customZone 660 []
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        , Task.perform GotTimeNow Time.now
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Calendar"
    , content =
        main_ []
            [ section [ class "top" ]
                [ div [ class "wrapper" ]
                    [ viewPageHeader model.timeZone model.timeNow
                    , div [ class "grid month-grid" ]
                        (List.map
                            (\a ->
                                div [ class "grid-day" ]
                                    [ span [ class "day" ] [ text (String.fromInt a) ]
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
                            (List.range 1 31)
                        )
                    ]
                ]
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
                class "active"

            else
                class ""

        viewMonth month =
            li [ isMonth month ] [ button [] [ text (Timestamp.getMonth month) ] ]
    in
    div [ class "page-header" ]
        [ ul [ class "nav nav-left" ] <|
            List.map viewMonth firstSixMonths
        , div [ class "page-title" ]
            [ h1 [] [ text "The Monthly Brief" ]
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



-- UPDATE


type Msg
    = NoOp
    | GotTimeZone Time.Zone
    | GotTimeNow Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTimeZone tz ->
            ( { model | timeZone = Time.customZone 660 [] }, Cmd.none )

        GotTimeNow time ->
            ( { model | timeNow = time }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
