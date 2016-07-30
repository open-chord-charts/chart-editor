module Components.ChartCard exposing (Model, Msg, update, view)

import Html exposing (..)
import Html.Attributes exposing (style)
import List.Split
import ChartTypes exposing (Chart, ChartKey(..), Part(..), Bar(..), Chord(..), Quality(..), PartName)


-- TYPES


type Row
    = Row (Maybe PartName) (List Bar)


toRows : Part -> List Row
toRows part =
    case part of
        Part name bars ->
            let
                barChunks =
                    List.Split.chunksOfLeft 8 bars
            in
                List.indexedMap
                    (\index barChunk ->
                        Row
                            (if index == 0 then
                                Just name
                             else
                                Nothing
                            )
                            barChunk
                    )
                    barChunks

        PartRepeat name ->
            [ Row (Just name) (List.repeat 8 BarRepeat) ]



-- MODEL


type alias Model =
    Chart



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


view : Model -> Html Msg
view { title, key, parts } =
    article [ style [ ( "width", "400px" ) ] ]
        [ h1 []
            [ text <| title ++ " (" ++ renderChartKey key ++ ")" ]
        , table
            [ style [ ( "border-collapse", "collapse" ) ] ]
            [ tbody [] <|
                (parts |> List.concatMap toRows |> List.map viewRow)
            ]
        ]


viewRow : Row -> Html Msg
viewRow (Row name bars) =
    tr []
        ([ td
            [ style [ ( "width", "1em" ) ] ]
            [ text <| Maybe.withDefault "" name ]
         ]
            ++ List.map viewBar bars
        )


viewBar : Bar -> Html Msg
viewBar bar =
    td
        [ style
            [ ( "border", "1px solid" )
            , ( "text-align", "center" )
            , ( "height", "2em" )
            , ( "width", "2.5em" )
            ]
        ]
        [ text <| renderBar bar ]



-- RENDER


renderChartKey : ChartKey -> String
renderChartKey (ChartKey key) =
    toString key


renderBar : Bar -> String
renderBar bar =
    case bar of
        Bar ( a, b, c, d ) ->
            renderChord a

        BarRepeat ->
            "–"


renderChord : Chord -> String
renderChord (Chord note quality) =
    toString note ++ renderQuality quality


renderQuality : Quality -> String
renderQuality quality =
    case quality of
        Major ->
            ""

        Minor ->
            "m"
