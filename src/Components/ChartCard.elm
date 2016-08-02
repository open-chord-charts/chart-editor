module Components.ChartCard exposing (Model, view)

import Html exposing (..)
import Html.Attributes exposing (style)
import List.Split
import Music.Chart exposing (..)
import Music.Chord as Chord exposing (..)
import Music.Note as Note


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



-- VIEW


view : Model -> Html msg
view { title, key, parts } =
    article [ style [ ( "width", "400px" ) ] ]
        [ h1 []
            [ text <| title ++ " (" ++ renderKey key ++ ")" ]
        , table
            [ style [ ( "border-collapse", "collapse" ) ] ]
            [ tbody [] (parts |> List.concatMap toRows |> List.map viewRow) ]
        ]


viewRow : Row -> Html msg
viewRow (Row name bars) =
    tr [] <|
        td
            [ style [ ( "width", "1em" ) ] ]
            [ text <| Maybe.withDefault "" name ]
            :: List.map viewBar bars


viewBar : Bar -> Html msg
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


renderBar : Bar -> String
renderBar bar =
    case bar of
        Bar ( a, b, c, d ) ->
            Chord.toString a

        BarRepeat ->
            "–"


renderKey : Key -> String
renderKey (Key note) =
    Note.toString note
