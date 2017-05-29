module Page.Home exposing (Model, initialModel, view)

import Html exposing (..)
import Data.Session as Session exposing (Session)
import Route


type alias Slug =
    String


type alias ChartInfos =
    { name : String
    , slug : Slug
    }


type alias Model =
    List ChartInfos


initialModel : Model
initialModel =
    [ { name = "All of me"
      , slug = "all-of-me"
      }
    , { name = "Wrong chart"
      , slug = "wrong-chart"
      }
    ]


view : Session -> Model -> Html msg
view session model =
    main_ []
        [ h1 []
            [ text "Homepage" ]
        , ul []
            (model
                |> List.map
                    (\{ name, slug } ->
                        li []
                            [ a [ Route.href (Route.Chart slug) ]
                                [ text name ]
                            ]
                    )
            )
        ]
