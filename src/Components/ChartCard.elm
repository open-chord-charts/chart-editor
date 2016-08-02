module Components.ChartCard exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Music.Chart as Chart exposing (Chart, Key(..))
import Music.Note as Note exposing (Note)
import Components.Chart as ChartComponent


-- MODEL


type alias Model =
    { chart : Chart
    , viewKey : Key
    }


init : Chart -> Model
init chart =
    { chart = chart
    , viewKey = chart.key
    }



-- MSG


type Msg
    = ChangeTransposedKey Key



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTransposedKey key ->
            { model | viewKey = key }



-- VIEW


view : Model -> Html Msg
view { chart, viewKey } =
    viewCard
        (chart.title ++ " (" ++ renderKey viewKey ++ ")")
        [ viewToolbar [ viewSelectKey viewKey ]
        , ChartComponent.view <| Chart.transpose viewKey chart
        ]


viewCard : String -> List (Html a) -> Html a
viewCard title children =
    article
        [ style
            [ ( "border", "1px solid" )
            , ( "margin", "1em" )
            , ( "padding", "1em" )
            , ( "width", "500px" )
            ]
        ]
    <|
        (h1 [] [ text title ])
            :: children


viewToolbar : List (Html msg) -> Html msg
viewToolbar children =
    div
        [ style
            [ ( "margin-top", "1em" )
            , ( "margin-bottom", "1em" )
            ]
        ]
        children


viewSelectKey : Key -> Html Msg
viewSelectKey key =
    label []
        [ text "Chart key: "
        , select
            [ on "change" <|
                Json.map ChangeTransposedKey <|
                    targetValue
                        `Json.andThen` noteDecoder
                        `Json.andThen` \note -> Json.succeed <| Key note
            ]
          <|
            List.map (viewSelectKeyOption key) Note.notes
        ]


viewSelectKeyOption : Key -> Note -> Html Msg
viewSelectKeyOption key note =
    let
        noteStr =
            Note.toString note
    in
        option
            [ selected <| (Key note) == key
            , value noteStr
            ]
            [ text noteStr ]



-- DECODER


noteDecoder : String -> Json.Decoder Note
noteDecoder val =
    case Note.fromString val of
        Just note ->
            Json.succeed note

        Nothing ->
            Json.fail <| "Value is not of type Note: " ++ val



-- RENDER


renderKey : Key -> String
renderKey (Key note) =
    Note.toString note
