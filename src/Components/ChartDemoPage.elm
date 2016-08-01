module Components.ChartDemoPage exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Components.ChartCard as ChartCard
import Music.Transposition exposing (transposeChart)
import Music.Types exposing (Chart, ChartKey(..), ChromaticNote(..))
import Samples


-- MODEL


type alias Model =
    { chart : Chart
    , transposedChartKey : ChartKey
    }


model : Model
model =
    { chart = Samples.allOfMe
    , transposedChartKey = ChartKey F
    }



-- MSG


type Msg
    = ChangeTransposedChartKey ChartKey



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTransposedChartKey chartKey ->
            { model | transposedChartKey = chartKey }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ ChartCard.view model.chart
        , hr [] []
        , ChartCard.view <| transposeChart model.transposedChartKey model.chart
        , label []
            [ text "Chart key: "
            , select
                [ on "change" <| Json.map ChangeTransposedChartKey <| targetValue `Json.andThen` chartKeyDecoder
                , style [ ( "margin-top", "1em" ) ]
                ]
              <|
                List.map viewSelectChartKeyOption [ Ab, A, Bb, B, C, Db, D, Eb, E, F, Gb, G ]
            ]
        ]


viewSelectChartKeyOption : ChromaticNote -> Html Msg
viewSelectChartKeyOption note =
    let
        noteStr =
            toString note
    in
        option
            [ selected <| (ChartKey note) == model.transposedChartKey
            , value noteStr
            ]
            [ text noteStr ]



-- DECODER


chartKeyDecoder : String -> Json.Decoder ChartKey
chartKeyDecoder val =
    case chromaticNoteFromString val of
        Just note ->
            Json.succeed <| ChartKey note

        Nothing ->
            Json.fail <| "Invalid ChartKey: " ++ val


chromaticNoteFromString : String -> Maybe ChromaticNote
chromaticNoteFromString string =
    case string of
        "Ab" ->
            Just Ab

        "A" ->
            Just A

        "Bb" ->
            Just Bb

        "B" ->
            Just B

        "C" ->
            Just C

        "Db" ->
            Just Db

        "D" ->
            Just D

        "Eb" ->
            Just Eb

        "E" ->
            Just E

        "F" ->
            Just F

        "Gb" ->
            Just Gb

        "G" ->
            Just G

        _ ->
            Nothing
