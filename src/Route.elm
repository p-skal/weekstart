module Route exposing (Route(..), back, fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Time
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Username exposing (Username)



-- ROUTING


type Route
    = Root
    | Login
    | Day Time.Posix
    | Week
    | Month


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Week Parser.top
        , Parser.map Login (s "login")
        , Parser.map Day (s "day" </> dayParser)
        , Parser.map Month (s "calendar")
        ]


dayParser : Parser (Time.Posix -> a) a
dayParser =
    Parser.custom "TIME"
        (\str ->
            let
                time =
                    Time.millisToPosix <| Maybe.withDefault 0 <| String.toInt str
            in
            Just time
        )



-- PUBLIC HELPERS


back : Nav.Key -> Cmd msg
back key =
    Nav.back key 1


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Week ->
                    []

                Root ->
                    []

                Login ->
                    [ "login" ]

                Day dayTime ->
                    [ "day", String.fromInt (Time.posixToMillis dayTime) ]

                Month ->
                    [ "calendar" ]
    in
    "#/" ++ String.join "/" pieces
