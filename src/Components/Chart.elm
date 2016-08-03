module Components.Chart exposing (Model, view)

import Html exposing (..)
import Html.Attributes exposing (style)
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
    Chart



-- VIEW


view : Maybe SelectedChord -> Model -> Html msg
view selectedChord { parts } =
    table
        [ style
            [ ( "border-collapse", "collapse" )
            , ( "width", "400px" )
            ]
        ]
        [ tbody [] (parts |> List.concatMap toRows |> List.map (viewRow selectedChord)) ]


viewRow : Maybe SelectedChord -> Row -> Html msg
viewRow selectedChord { partName, isFromRepeatPart, bars } =
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
                (\index bar ->
                    let
                        isBarSelected =
                            (case partName of
                                Just n ->
                                    (case selectedChord of
                                        Just { partName, barIndex } ->
                                            not isFromRepeatPart && partName == n && barIndex == index

                                        Nothing ->
                                            False
                                    )

                                Nothing ->
                                    False
                            )
                    in
                        viewBar isBarSelected bar
                )
                bars


viewBar : Bool -> Bar -> Html msg
viewBar selected bar =
    td
        [ style
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
