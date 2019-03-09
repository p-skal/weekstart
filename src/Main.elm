module Main exposing (main, view)

import Api exposing (Cred)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Day as Day
import Page.Login as Login
import Page.Month as Month
import Page.NotFound as NotFound
import Page.Week as Week
import Route exposing (Route)
import Session exposing (Session)
import Time
import Url exposing (Url)
import Viewer exposing (Viewer)
import WeekTask exposing (WeekTask)


type alias Model =
    { appState : AppState
    , tasks : List WeekTask
    }


type AppState
    = Day Day.Model
    | Week Week.Model
    | Login Login.Model
    | Month Month.Model
    | NotFound Session
    | Redirect Session



-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    let
        initTime =
            Time.millisToPosix 0

        ausZone =
            Time.customZone 660 []
    in
    changeRouteTo (Route.fromUrl url)
        { appState = Redirect (Session.fromViewer navKey maybeViewer)
        , tasks =
            [ WeekTask Time.Mon "Get up for uni" initTime WeekTask.Important
            , WeekTask Time.Tue "Test Weekstart" (Time.millisToPosix 1552093200000) WeekTask.Important
            ]
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view (Session.viewer (toSession model)) page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model.appState of
        Redirect _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        Login login ->
            viewPage Page.Other GotLoginMsg (Login.view login)

        Day day ->
            viewPage Page.Day GotDayMsg (Day.view day model.tasks)

        Week week ->
            viewPage Page.Week GotWeekMsg (Week.view week model.tasks)

        Month month ->
            viewPage Page.Month GotMonthMsg (Month.view month model.tasks)



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotDayMsg Day.Msg
    | GotWeekMsg Week.Msg
    | GotLoginMsg Login.Msg
    | GotMonthMsg Month.Msg
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page.appState of
        Redirect session ->
            session

        NotFound session ->
            session

        Day day ->
            Day.toSession day

        Week week ->
            Week.toSession week

        Login login ->
            Login.toSession login

        Month month ->
            Month.toSession month


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( { model | appState = NotFound session }, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Week )

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just (Route.Day dayTime) ->
            Day.init session dayTime model.tasks
                |> updateWith Day GotDayMsg model

        Just Route.Week ->
            Week.init session model.tasks
                |> updateWith Week GotWeekMsg model

        Just Route.Month ->
            Month.init session model.tasks
                |> updateWith Month GotMonthMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.appState ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotDayMsg subMsg, Day day ) ->
            Day.update subMsg day
                |> updateWith Day GotDayMsg model

        ( GotWeekMsg subMsg, Week week ) ->
            Week.update subMsg week
                |> updateWith Week GotWeekMsg model

        ( GotMonthMsg subMsg, Month month ) ->
            Month.update subMsg month
                |> updateWith Month GotMonthMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> AppState) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    let
        tasks =
            case toModel subModel of
                Day day ->
                    day.tasks

                Week week ->
                    week.tasks

                Month month ->
                    month.tasks

                _ ->
                    model.tasks
    in
    ( { model | appState = toModel subModel, tasks = tasks }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.appState of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Day day ->
            Sub.map GotDayMsg (Day.subscriptions day)

        Week week ->
            Sub.map GotWeekMsg (Week.subscriptions week)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Month month ->
            Sub.map GotMonthMsg (Month.subscriptions month)



-- MAIN


main : Program Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
