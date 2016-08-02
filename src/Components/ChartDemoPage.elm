module Components.ChartDemoPage exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Components.ChartCard as ChartCard
import Music.Chart as Chart exposing (Chart, Key(..))
import Music.Note as Note exposing (Note)
import Samples


-- MODEL


type alias Model =
    { chart : Chart
    , transposedKey : Key
    }


model : Model
model =
    { chart = Samples.allOfMe
    , transposedKey = Key Note.noteF
    }



-- MSG


type Msg
    = ChangeTransposedKey Key



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTransposedKey key ->
            { model | transposedKey = key }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ ChartCard.view model.chart
        , hr [] []
        , ChartCard.view <| Chart.transpose model.transposedKey model.chart
        , label []
            [ text "Chart key: "
            , select
                [ on "change" <|
                    Json.map ChangeTransposedKey <|
                        targetValue
                            `Json.andThen` noteDecoder
                            `Json.andThen` \note -> Json.succeed <| Key note
                , style [ ( "margin-top", "1em" ) ]
                ]
              <|
                List.map viewSelectKeyOption Note.notes
            ]
        ]


viewSelectKeyOption : Note -> Html Msg
viewSelectKeyOption note =
    let
        noteStr =
            Note.toString note
    in
        option
            [ selected <| (Key note) == model.transposedKey
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
