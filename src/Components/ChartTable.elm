module Components.ChartTable exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Split
import Music.Chart exposing (..)
import Music.Chord as Chord exposing (..)
import Components.Types exposing (SelectedChord)


-- TYPES


type alias Row =
    { partName : Maybe PartName
    , isFromRepeatPart : Bool
    , bars : List Bar
    }


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
                            False
                            barChunk
                    )
                    barChunks

        PartRepeat name ->
            [ Row (Just name) True (List.repeat 8 BarRepeat) ]



-- MODEL


type alias Model =
    { chart : Chart
    , selectedChord : Maybe SelectedChord
    }


init : Chart -> Model
init chart =
    { chart = chart
    , selectedChord = Nothing
    }



-- MSG


type Msg
    = SelectChord Int Int



-- UPDATE


update : Msg -> Model -> Model
update msg ({ selectedChord } as model) =
    case msg of
        SelectChord rowIndex barIndex ->
            { model
                | selectedChord =
                    case selectedChord of
                        Just s ->
                            Just { s | barIndex = barIndex, partName = "A" }

                        Nothing ->
                            Nothing
            }



-- VIEW


view : Model -> Html Msg
view { chart, selectedChord } =
    table
        [ style
            [ ( "border-collapse", "collapse" )
            , ( "width", "400px" )
            ]
        ]
        [ tbody []
            (chart.parts
                |> List.concatMap toRows
                |> List.indexedMap (viewRow selectedChord)
            )
        ]


viewRow : Maybe SelectedChord -> Int -> Row -> Html Msg
viewRow selectedChord rowIndex { partName, isFromRepeatPart, bars } =
    tr
        [ style
            (if isFromRepeatPart then
                []
             else
                [ ( "height", "2em" ) ]
            )
        ]
    <|
        (td
            [ style [ ( "width", "1em" ) ] ]
            [ text <| Maybe.withDefault "" partName ]
        )
            :: List.indexedMap
                (\barIndex bar ->
                    let
                        isBarSelected =
                            (case partName of
                                Just partName ->
                                    (case selectedChord of
                                        Just s ->
                                            not isFromRepeatPart && partName == s.partName && barIndex == s.barIndex

                                        Nothing ->
                                            False
                                    )

                                Nothing ->
                                    False
                            )
                    in
                        viewBar isBarSelected rowIndex barIndex bar
                )
                bars


viewBar : Bool -> Int -> Int -> Bar -> Html Msg
viewBar selected rowIndex barIndex bar =
    td
        [ onClick <| SelectChord rowIndex barIndex
        , style
            [ ( "background-color"
              , if selected then
                    "lightgray"
                else
                    ""
              )
            , ( "border", "1px solid" )
            , ( "height", "inherit" )
            , ( "padding", "0" )
            , ( "text-align", "center" )
            , ( "vertical-align", "middle" )
            , ( "width", "2.5em" )
            ]
        ]
        [ text <| renderBar bar ]



-- RENDER


renderBar : Bar -> String
renderBar bar =
    case bar of
        Bar ( a, b, c, d ) ->
            Chord.toString a

        BarRepeat ->
            "–"
