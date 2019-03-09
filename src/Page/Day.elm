module Page.Day exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Asset exposing (btnIconLeft, btnIconRight)
import Derberos.Date.Calendar as Calendar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Page
import Route
import Session exposing (Session)
import Task
import Time
import Timestamp
import WeekTask exposing (WeekTask)



-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , timeNow : Time.Posix
    , viewDay : Time.Posix
    , tasks : List WeekTask
    }



-- INIT


init : Session -> Time.Posix -> List WeekTask -> ( Model, Cmd Msg )
init session dayTime tasks =
    let
        initTime =
            Time.millisToPosix 0

        ausZone =
            Time.customZone 660 []
    in
    ( { session = session
      , timeZone = ausZone
      , timeNow = initTime
      , viewDay = dayTime
      , tasks = tasks
      }
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        , Task.perform GotTimeNow Time.now
        ]
    )



-- VIEW


view : Model -> List WeekTask -> { title : String, content : Html Msg }
view model tasks =
    { title = "Week"
    , content =
        main_ []
            [ section [ class "top" ]
                [ div [ class "wrapper" ]
                    [ div [ style "display" "flex", style "height" "100%" ]
                        [ section [ style "marginRight" "1rem" ]
                            [ h1 [ style "marginBottom" "0" ] [ text <| Timestamp.getDay <| Time.toWeekday model.timeZone model.viewDay ]
                            , h3 [] [ Timestamp.view model.timeZone model.viewDay ]
                            , br [] []
                            , tasks
                                |> List.filter (\task -> Time.toDay model.timeZone task.time == Time.toDay model.timeZone model.viewDay)
                                |> List.filter (\task -> Time.toMonth model.timeZone task.time == Time.toMonth model.timeZone model.viewDay)
                                |> List.filter (\task -> Time.toYear model.timeZone task.time == Time.toYear model.timeZone model.viewDay)
                                |> WeekTask.viewList model.timeZone model.timeNow model.viewDay EditingTask
                            , br [] []
                            , button [ class "btn btn-secondary", onClick PreviousPage ] [ btnIconLeft "arrow-left", text "Back" ]
                            ]
                        , section [ style "display" "flex", style "justify-content" "center", style "align-items" "center", style "background" "rgba(0, 0, 0, 0.025)", style "margin-left" "1rem" ]
                            [ h2 [] [ text "No event selected." ]
                            ]
                        ]
                    ]
                ]
            ]
    }



-- UPDATE


type Msg
    = GotTimeZone Time.Zone
    | GotTimeNow Time.Posix
    | Tick Time.Posix
    | EditingTask WeekTask String
    | PreviousPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeZone tz ->
            ( { model | timeZone = Time.customZone 660 [] }, Cmd.none )

        GotTimeNow time ->
            ( { model | timeNow = time }
            , Cmd.none
            )

        Tick newTime ->
            ( { model | timeNow = newTime }, Cmd.none )

        EditingTask details tas ->
            ( model, Cmd.none )

        PreviousPage ->
            ( model, Route.back (Session.navKey model.session) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
