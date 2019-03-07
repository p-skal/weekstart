module Page.NotFound exposing (view)

import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (class, id, src, tabindex)



-- VIEW


view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content =
        main_ []
            [ div [ class "wrapper" ]
                [ h1 [] [ text "Not Found" ]
                , div [ class "row" ]
                    [ text "There was an error." ]
                ]
            ]
    }
