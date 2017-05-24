module Page.NotFound exposing (view)

import Html exposing (Html, main_, h1, div, img, text)
import Data.Session as Session exposing (Session)


-- VIEW --


view : Session -> Html msg
view session =
    main_ []
        [ h1 [] [ text "Not Found" ] ]
