module ChartCard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Music.Chart exposing (..)
import Music.Chord exposing (..)
import Music.Note exposing (..)
import Parser
import Parsers
import String
import Svg exposing (svg)
import Svg.Attributes


-- CONSTANTS


defaultChord : Chord
defaultChord =
    Chord C Major


defaultBar : Bar
defaultBar =
    Bar [ defaultChord ]


defaultPart : Part
defaultPart =
    Part "" [ defaultBar ]


nbBarsByRow : Int
nbBarsByRow =
    8


nbMaxChordsInBar : Int
nbMaxChordsInBar =
    4


youFoundABugMessage : String
youFoundABugMessage =
    "Should never happen – you found a bug :-)"



-- TYPES


type alias BarIndex =
    Int


type alias ChordIndex =
    Int


type alias RowIndex =
    Int



-- MODEL


type alias BarReference =
    { partIndex : PartIndex
    , barIndex : BarIndex
    }


type Selection
    = PartSelection PartIndex
    | BarSelection BarReference


getBarAtReference : BarReference -> Chart -> Maybe Bar
getBarAtReference barReference chart =
    getBarsOfPartByIndex barReference.partIndex chart
        |> List.getAt barReference.barIndex


updateBarAt : BarReference -> (Bar -> Bar) -> Chart -> Chart
updateBarAt barReference updateBar chart =
    updatePartAtIndex barReference.partIndex
        (mapPartBars
            (List.indexedMap
                (\barIndex bar ->
                    if barIndex == barReference.barIndex then
                        updateBar bar
                    else
                        bar
                )
            )
        )
        chart


updatePartAtIndex : PartIndex -> (Part -> Part) -> Chart -> Chart
updatePartAtIndex partIndex updatePart chart =
    let
        newParts =
            chart.parts
                |> List.indexedMap
                    (\partIndex1 part ->
                        if partIndex == partIndex1 then
                            updatePart part
                        else
                            part
                    )
    in
        { chart | parts = newParts }


removePartAtIndex : PartIndex -> Chart -> Chart
removePartAtIndex partIndex chart =
    let
        newParts =
            chart.parts
                |> List.indexedMap (,)
                |> List.filterMap
                    (\( partIndex1, part ) ->
                        if partIndex == partIndex1 then
                            Nothing
                        else
                            Just part
                    )
    in
        { chart | parts = newParts }


type ChartStatus
    = ViewStatus
    | EditStatus Selection


type alias Model =
    { chart : Chart
    , chartStr : String
    , status : ChartStatus
    , viewedKey : Key
    }


init : Chart -> Model
init chart =
    { chart = chart
    , chartStr = Music.Chart.toString chart
    , status = ViewStatus
    , viewedKey = chart.key
    }



-- MSG


type Msg
    = AddBar BarReference
    | AddChord BarReference
    | AddPart PartIndex
    | DuplicatePart PartIndex
    | Edit
    | MovePart PartIndex PartIndex
    | RemoveBar BarReference
    | RemoveChord BarReference ChordIndex
    | RemovePart PartIndex
    | Save
    | SelectBar BarReference
    | SelectPart PartIndex
    | SetBarRepeat BarReference Bool
    | SetPartName PartIndex PartName
    | SetPartRepeat PartIndex Bool
    | SetChord BarReference ChordIndex Chord
    | SetViewKey Key
    | TextAreaInput String
    | TextAreaSave



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        addPart : PartIndex -> Part -> Model -> Model
        addPart partIndex part model =
            let
                newParts =
                    model.chart.parts
                        |> List.splitAt partIndex
                        |> (\( init, tail ) -> init ++ [ part ] ++ tail)

                chart =
                    model.chart

                newChart =
                    { chart | parts = newParts }

                newStatus =
                    EditStatus (PartSelection partIndex)
            in
                { model | status = newStatus }
                    |> updateChartAndChartStr newChart

        parse : String -> Model
        parse chartStr =
            case Parser.run Parsers.chart chartStr of
                Ok chart ->
                    { model | chart = chart }

                Err err ->
                    let
                        _ =
                            Debug.log "Error parsing chart" err
                    in
                        model

        removePart : PartIndex -> Model -> Model
        removePart partIndex model =
            let
                newChart =
                    model.chart
                        |> removePartAtIndex partIndex

                nbParts =
                    List.length newChart.parts

                newStatus =
                    EditStatus
                        (PartSelection
                            (if partIndex > nbParts - 1 then
                                Basics.max 0 (nbParts - 1)
                             else
                                partIndex
                            )
                        )
            in
                { model | status = newStatus }
                    |> updateChartAndChartStr newChart

        updateChartAndChartStr : Chart -> Model -> Model
        updateChartAndChartStr chart model =
            { model
                | chart = chart
                , chartStr = Music.Chart.toString chart
            }
    in
        case msg of
            AddBar barReference ->
                let
                    newChart =
                        model.chart
                            |> updatePartAtIndex barReference.partIndex
                                (mapPartBars
                                    (List.splitAt barReference.barIndex
                                        >> (\( init, tail ) -> init ++ [ defaultBar ] ++ tail)
                                    )
                                )

                    newStatus =
                        EditStatus (BarSelection barReference)
                in
                    { model | status = newStatus }
                        |> updateChartAndChartStr newChart

            AddChord barReference ->
                let
                    newChart =
                        model.chart
                            |> updateBarAt barReference (mapBarChords (\chords -> chords ++ [ defaultChord ]))
                in
                    model |> updateChartAndChartStr newChart

            AddPart partIndex ->
                addPart partIndex defaultPart model

            DuplicatePart partIndex ->
                case List.getAt partIndex model.chart.parts of
                    Nothing ->
                        model

                    Just part ->
                        addPart (partIndex + 1) part model

            Edit ->
                let
                    newStatus =
                        EditStatus (BarSelection { partIndex = 0, barIndex = 0 })
                in
                    { model | status = newStatus }

            MovePart fromPartIndex toPartIndex ->
                case List.getAt fromPartIndex model.chart.parts of
                    Nothing ->
                        model

                    Just part ->
                        model
                            |> removePart fromPartIndex
                            |> addPart toPartIndex part

            RemoveBar barReference ->
                let
                    newChart =
                        model.chart
                            |> updatePartAtIndex barReference.partIndex
                                (mapPartBars (List.removeAt barReference.barIndex))

                    nbBars =
                        getBarsOfPartByIndex barReference.partIndex newChart
                            |> List.length

                    newBarIndex =
                        if barReference.barIndex > nbBars - 1 then
                            Basics.max 0 (nbBars - 1)
                        else
                            barReference.barIndex

                    newStatus =
                        EditStatus
                            (BarSelection { barReference | barIndex = newBarIndex })
                in
                    { model | status = newStatus }
                        |> updateChartAndChartStr newChart

            RemoveChord barReference chordIndex ->
                let
                    newChart =
                        model.chart
                            |> updateBarAt barReference (mapBarChords (List.removeAt chordIndex))
                in
                    model |> updateChartAndChartStr newChart

            RemovePart partIndex ->
                removePart partIndex model

            Save ->
                parse model.chartStr
                    |> (\model -> { model | status = ViewStatus })

            SelectBar barReference ->
                let
                    newStatus =
                        EditStatus (BarSelection barReference)
                in
                    { model | status = newStatus }

            SelectPart partIndex ->
                let
                    newStatus =
                        EditStatus (PartSelection partIndex)
                in
                    { model | status = newStatus }

            SetBarRepeat barReference checked ->
                let
                    newChart =
                        model.chart
                            |> updateBarAt barReference
                                (\_ ->
                                    if checked then
                                        BarRepeat
                                    else
                                        defaultBar
                                )
                in
                    model |> updateChartAndChartStr newChart

            SetPartName partIndex partName ->
                let
                    newChart =
                        model.chart
                            |> updatePartAtIndex partIndex
                                (\part ->
                                    case part of
                                        Part _ bars ->
                                            Part partName bars

                                        PartRepeat _ ->
                                            PartRepeat partName
                                )
                in
                    model |> updateChartAndChartStr newChart

            SetPartRepeat partIndex checked ->
                let
                    newChart =
                        model.chart
                            |> updatePartAtIndex partIndex
                                (\part ->
                                    let
                                        partName =
                                            getPartName part
                                    in
                                        if checked then
                                            PartRepeat partName
                                        else
                                            Part partName [ defaultBar ]
                                )
                in
                    model |> updateChartAndChartStr newChart

            SetChord barReference chordIndex chord ->
                let
                    newChart =
                        model.chart
                            |> updateBarAt barReference
                                (mapBarChords
                                    (List.indexedMap
                                        (\chordIndex1 chord1 ->
                                            if chordIndex == chordIndex1 then
                                                chord
                                            else
                                                chord1
                                        )
                                    )
                                )
                in
                    model |> updateChartAndChartStr newChart

            SetViewKey key ->
                { model | viewedKey = key }

            TextAreaInput str ->
                { model | chartStr = str }

            TextAreaSave ->
                parse model.chartStr



-- VIEW


view : Model -> Html Msg
view { chart, chartStr, status, viewedKey } =
    card chart.title
        (keyToString chart.key)
        [ div [ class "dt dt--fixed collapse mv3 athelas" ]
            (let
                viewedChart =
                    case status of
                        EditStatus _ ->
                            chart

                        ViewStatus ->
                            chart
                                |> Music.Chart.transpose viewedKey
             in
                viewedChart.parts
                    |> List.indexedMap (viewPart chart status)
                    |> List.concat
            )
        , case status of
            EditStatus selection ->
                div []
                    [ button Primary
                        NotPressed
                        [ class "mr1"
                        , onClick Save
                        ]
                        [ text "Save" ]
                    , button Secondary
                        NotPressed
                        [ onClick TextAreaSave ]
                        [ text "Parse" ]
                    , case selection of
                        BarSelection barReference ->
                            case getBarAtReference barReference chart of
                                Nothing ->
                                    text youFoundABugMessage

                                Just selectedBar ->
                                    viewBarEditor chart barReference selectedBar

                        PartSelection partIndex ->
                            case List.getAt partIndex chart.parts of
                                Nothing ->
                                    text youFoundABugMessage

                                Just part ->
                                    viewPartEditor chart partIndex part
                    , textarea
                        [ cols 80
                        , rows ((chartStr |> String.lines |> List.length) + 5)
                        , onInput TextAreaInput
                        , spellcheck False
                        , value chartStr
                        ]
                        []
                    ]

            ViewStatus ->
                div []
                    [ toolbar
                        [ label []
                            [ text "Transpose to: "
                            , let
                                (Key viewedNote) =
                                    viewedKey
                              in
                                noteSelect Music.Note.notes viewedNote (Key >> SetViewKey)
                            ]
                        ]
                    , button Primary
                        NotPressed
                        [ onClick Edit ]
                        [ text "Edit" ]
                    ]
        ]


viewBarEditor : Chart -> BarReference -> Bar -> Html Msg
viewBarEditor chart barReference bar =
    let
        barRepeatCheckbox isChecked =
            label []
                [ input
                    [ checked isChecked
                    , onCheck (SetBarRepeat barReference)
                    , type_ "checkbox"
                    ]
                    []
                , text " repeated bar"
                ]
    in
        div []
            ((case bar of
                Bar chords ->
                    [ toolbar [ barRepeatCheckbox False ] ]
                        ++ (chords
                                |> List.indexedMap
                                    (\chordIndex (Chord note quality) ->
                                        toolbar
                                            [ noteSelect Music.Note.notes
                                                note
                                                (\selectedNote ->
                                                    SetChord barReference chordIndex (Chord selectedNote quality)
                                                )
                                            , qualitySelect quality
                                                (\selectedQuality ->
                                                    SetChord barReference chordIndex (Chord note selectedQuality)
                                                )
                                            , button Secondary
                                                NotPressed
                                                [ disabled (List.length chords == 1)
                                                , onClick (RemoveChord barReference chordIndex)
                                                , class "ml1"
                                                ]
                                                [ text "Remove chord" ]
                                            ]
                                    )
                           )
                        ++ [ toolbar
                                [ button Secondary
                                    NotPressed
                                    [ disabled (List.length chords == nbMaxChordsInBar)
                                    , onClick (AddChord barReference)
                                    ]
                                    [ text "Add chord in bar" ]
                                ]
                           ]

                BarRepeat ->
                    [ toolbar
                        [ barRepeatCheckbox True ]
                    ]
             )
                ++ [ toolbar
                        [ button Secondary
                            NotPressed
                            [ class "mr1"
                            , onClick (AddBar barReference)
                            ]
                            [ text "Add bar before" ]
                        , button Secondary
                            NotPressed
                            [ class "mr1"
                            , onClick (AddBar { barReference | barIndex = barReference.barIndex + 1 })
                            ]
                            [ text "Add bar after" ]
                        , let
                            isLastBarInPart =
                                (getBarsOfPartByIndex barReference.partIndex chart |> List.length) == 1
                          in
                            button Secondary
                                NotPressed
                                [ class "mr1"
                                , disabled isLastBarInPart
                                , onClick (RemoveBar barReference)
                                ]
                                [ text "Remove bar" ]
                        ]
                   ]
            )


viewPartEditor : Chart -> PartIndex -> Part -> Html Msg
viewPartEditor chart partIndex part =
    let
        partRepeatCheckbox isChecked =
            label []
                [ input
                    [ checked isChecked
                    , onCheck (SetPartRepeat partIndex)
                    , type_ "checkbox"
                    ]
                    []
                , text " repeated part"
                ]
    in
        div []
            ([ toolbar
                [ partRepeatCheckbox (isPartRepeat part)
                ]
             , toolbar
                [ input
                    [ class "pa2 w3 ba br2 b--mid-gray"
                    , onInput (SetPartName partIndex)
                    , value (getPartName part)
                    ]
                    []
                ]
             ]
                ++ (case part of
                        PartRepeat _ ->
                            []

                        Part _ bars ->
                            [ toolbar
                                [ button Secondary
                                    NotPressed
                                    [ class "mr1"
                                    , onClick (AddBar (BarReference partIndex 0))
                                    ]
                                    [ text "Add bar at start" ]
                                , button Secondary
                                    NotPressed
                                    [ class "mr1"
                                    , onClick (AddBar (BarReference partIndex (List.length bars)))
                                    ]
                                    [ text "Add bar at end" ]
                                ]
                            ]
                   )
                ++ [ toolbar
                        [ button Secondary
                            NotPressed
                            [ class "mr1"
                            , onClick (AddPart partIndex)
                            ]
                            [ text "Add part before" ]
                        , button Secondary
                            NotPressed
                            [ class "mr1"
                            , onClick (AddPart (partIndex + 1))
                            ]
                            [ text "Add part after" ]
                        , button Secondary
                            NotPressed
                            [ class "mr1"
                            , onClick (DuplicatePart partIndex)
                            ]
                            [ text "Duplicate part" ]
                        , button Secondary
                            NotPressed
                            [ class "mr1"
                            , disabled (partIndex == 0)
                            , onClick (MovePart partIndex (partIndex - 1))
                            ]
                            [ text "Move part up" ]
                        , button Secondary
                            NotPressed
                            [ class "mr1"
                            , disabled (partIndex == List.length chart.parts - 1)
                            , onClick (MovePart partIndex (partIndex + 1))
                            ]
                            [ text "Move part down" ]
                        , button Secondary
                            NotPressed
                            [ class "mr1"
                            , disabled (List.length chart.parts == 1)
                            , onClick (RemovePart partIndex)
                            ]
                            [ text "Remove part" ]
                        ]
                   ]
            )


noteSelect : List Note -> Note -> (Note -> Msg) -> Html Msg
noteSelect notes selectedNote noteToMsg =
    select
        [ on "change"
            (targetValue
                |> Decode.andThen noteDecoder
                |> Decode.map noteToMsg
            )
        ]
        (notes
            |> List.map
                (\note ->
                    let
                        noteStr =
                            Music.Note.toString note
                    in
                        option
                            [ selected (note == selectedNote)
                            , value noteStr
                            ]
                            [ text noteStr ]
                )
        )


qualitySelect : Quality -> (Quality -> Msg) -> Html Msg
qualitySelect selectedQuality qualityToMsg =
    select
        [ on "change"
            (targetValue
                |> Decode.andThen qualityDecoder
                |> Decode.map qualityToMsg
            )
        ]
        (Music.Chord.qualities
            |> List.map
                (\quality ->
                    let
                        qualityStr =
                            Music.Chord.qualityToString quality
                    in
                        option
                            [ selected (quality == selectedQuality)
                            , value qualityStr
                            ]
                            [ text qualityStr ]
                )
        )


viewPart : Chart -> ChartStatus -> PartIndex -> Part -> List (Html Msg)
viewPart chart status partIndex part =
    let
        isPartSelected =
            case status of
                EditStatus selection ->
                    case selection of
                        BarSelection _ ->
                            False

                        PartSelection partIndex1 ->
                            partIndex == partIndex1

                ViewStatus ->
                    False

        partCell s =
            div
                ((case status of
                    EditStatus _ ->
                        [ onClick (SelectPart partIndex) ]

                    ViewStatus ->
                        []
                 )
                    ++ [ class "dtc w2 v-mid tc sans-serif"
                       , classList [ ( "bg-moon-gray", isPartSelected ) ]
                       ]
                )
                [ text s ]

        isBarSelected : BarIndex -> Bool
        isBarSelected barIndex =
            case status of
                EditStatus selection ->
                    case selection of
                        BarSelection barReference ->
                            partIndex == barReference.partIndex && barIndex == barReference.barIndex

                        PartSelection partIndex1 ->
                            partIndex == partIndex1

                ViewStatus ->
                    False
    in
        case part of
            Part partName bars ->
                bars
                    |> List.greedyGroupsOf nbBarsByRow
                    |> List.indexedMap
                        (\rowIndex rowBars ->
                            div [ class "dt-row" ]
                                (partCell
                                    (if rowIndex == 0 then
                                        partName
                                     else
                                        ""
                                    )
                                    :: (let
                                            nonEmptyBars =
                                                rowBars
                                                    |> List.indexedMap
                                                        (\barIndexInRow bar ->
                                                            let
                                                                barIndex =
                                                                    rowIndex * nbBarsByRow + barIndexInRow

                                                                previousBar =
                                                                    rowBars
                                                                        |> List.getAt (barIndexInRow - 1)
                                                            in
                                                                viewBar
                                                                    status
                                                                    (isBarSelected barIndex)
                                                                    (SelectBar (BarReference partIndex barIndex))
                                                                    bar
                                                                    previousBar
                                                        )

                                            nbPaddingBars =
                                                nbBarsByRow - List.length nonEmptyBars

                                            emptyBar =
                                                div [ class "dtc" ]
                                                    [ barCell [] ]

                                            paddingBars =
                                                List.repeat nbPaddingBars emptyBar
                                        in
                                            case status of
                                                EditStatus _ ->
                                                    nonEmptyBars ++ paddingBars

                                                ViewStatus ->
                                                    paddingBars ++ nonEmptyBars
                                       )
                                )
                        )

            PartRepeat partName ->
                [ div [ class "dt-row h2" ]
                    (partCell partName
                        :: (List.repeat nbBarsByRow BarRepeat
                                |> List.indexedMap
                                    (\barIndex bar ->
                                        viewBar status (isBarSelected barIndex) (SelectPart partIndex) bar Nothing
                                    )
                           )
                    )
                ]


viewBar : ChartStatus -> Bool -> Msg -> Bar -> Maybe Bar -> Html Msg
viewBar status isSelected msg bar previousBar =
    let
        defaultFontSizeClasses =
            "f7 f4-m f2-l"

        fontSizeClasses =
            case bar of
                Bar chords ->
                    if List.length chords > 1 then
                        "f7 f6-m f4-l"
                    else
                        defaultFontSizeClasses

                BarRepeat ->
                    defaultFontSizeClasses
    in
        div
            ([ class
                ("dtc ba b--mid-gray "
                    ++ fontSizeClasses
                    ++ " "
                    ++ (case status of
                            ViewStatus ->
                                "cursor-default"

                            EditStatus _ ->
                                "pointer"
                       )
                )
             , classList [ ( "bg-moon-gray", isSelected ) ]
             ]
                ++ (case status of
                        EditStatus _ ->
                            [ onClick msg ]

                        ViewStatus ->
                            []
                   )
            )
            [ let
                barCellWithSvg children =
                    barCell
                        [ svg [ Svg.Attributes.class "h-100 w-100 v-mid" ]
                            children
                        ]
              in
                case bar of
                    Bar chords ->
                        case chords of
                            [] ->
                                text ""

                            [ chord ] ->
                                barCellWithSvg
                                    [ Svg.text_
                                        [ Svg.Attributes.x "50%"
                                        , Svg.Attributes.y "50%"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord) ]
                                    ]

                            [ chord1, chord2 ] ->
                                barCellWithSvg
                                    [ Svg.line
                                        [ Svg.Attributes.x1 "0"
                                        , Svg.Attributes.y1 "100%"
                                        , Svg.Attributes.x2 "100%"
                                        , Svg.Attributes.y2 "0"
                                        , Svg.Attributes.class "stroke-mid-gray"
                                        ]
                                        []
                                    , Svg.text_
                                        [ Svg.Attributes.x "25%"
                                        , Svg.Attributes.y "25%"
                                        , Svg.Attributes.dy "0.1em"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord1) ]
                                    , Svg.text_
                                        [ Svg.Attributes.x "75%"
                                        , Svg.Attributes.y "75%"
                                        , Svg.Attributes.dy "-0.1em"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord2) ]
                                    ]

                            [ chord1, chord2, chord3 ] ->
                                barCellWithSvg
                                    [ Svg.line
                                        [ Svg.Attributes.x1 "0"
                                        , Svg.Attributes.y1 "50%"
                                        , Svg.Attributes.x2 "100%"
                                        , Svg.Attributes.y2 "50%"
                                        , Svg.Attributes.class "stroke-mid-gray"
                                        ]
                                        []
                                    , Svg.line
                                        [ Svg.Attributes.x1 "50%"
                                        , Svg.Attributes.y1 "50%"
                                        , Svg.Attributes.x2 "50%"
                                        , Svg.Attributes.y2 "100%"
                                        , Svg.Attributes.class "stroke-mid-gray"
                                        ]
                                        []
                                    , Svg.text_
                                        [ Svg.Attributes.x "50%"
                                        , Svg.Attributes.y "25%"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord1) ]
                                    , Svg.text_
                                        [ Svg.Attributes.x "25%"
                                        , Svg.Attributes.y "75%"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord2) ]
                                    , Svg.text_
                                        [ Svg.Attributes.x "75%"
                                        , Svg.Attributes.y "75%"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord3) ]
                                    ]

                            [ chord1, chord2, chord3, chord4 ] ->
                                barCellWithSvg
                                    [ Svg.line
                                        [ Svg.Attributes.x1 "0"
                                        , Svg.Attributes.y1 "50%"
                                        , Svg.Attributes.x2 "100%"
                                        , Svg.Attributes.y2 "50%"
                                        , Svg.Attributes.class "stroke-mid-gray"
                                        ]
                                        []
                                    , Svg.line
                                        [ Svg.Attributes.x1 "50%"
                                        , Svg.Attributes.y1 "0"
                                        , Svg.Attributes.x2 "50%"
                                        , Svg.Attributes.y2 "100%"
                                        , Svg.Attributes.class "stroke-mid-gray"
                                        ]
                                        []
                                    , Svg.text_
                                        [ Svg.Attributes.x "25%"
                                        , Svg.Attributes.y "25%"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord1) ]
                                    , Svg.text_
                                        [ Svg.Attributes.x "75%"
                                        , Svg.Attributes.y "25%"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord2) ]
                                    , Svg.text_
                                        [ Svg.Attributes.x "25%"
                                        , Svg.Attributes.y "75%"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord3) ]
                                    , Svg.text_
                                        [ Svg.Attributes.x "75%"
                                        , Svg.Attributes.y "75%"
                                        , Svg.Attributes.textAnchor "middle"
                                        , Svg.Attributes.dominantBaseline "central"
                                        ]
                                        [ Svg.text (Music.Chord.toString chord4) ]
                                    ]

                            _ ->
                                text "Error"

                    BarRepeat ->
                        barCellWithSvg
                            [ Svg.text_
                                [ Svg.Attributes.x "50%"
                                , Svg.Attributes.y "50%"
                                , Svg.Attributes.textAnchor "middle"
                                , Svg.Attributes.dominantBaseline "central"
                                ]
                                [ Svg.text
                                    (let
                                        dash =
                                            "–"
                                     in
                                        case previousBar of
                                            Nothing ->
                                                dash

                                            Just bar ->
                                                case bar of
                                                    Bar chords ->
                                                        if List.length chords == 2 then
                                                            "٪"
                                                        else
                                                            dash

                                                    BarRepeat ->
                                                        dash
                                    )
                                ]
                            ]
            ]


barCell : List (Html msg) -> Html msg
barCell children =
    div [ class "aspect-ratio aspect-ratio--4x3" ]
        [ div [ class "aspect-ratio--object" ]
            children
        ]



-- WIDGETS


type Purpose
    = Primary
    | Secondary


type ButtonState
    = Pressed
    | NotPressed


button : Purpose -> ButtonState -> List (Attribute msg) -> List (Html msg) -> Html msg
button purpose state attributes =
    Html.button
        (attributes
            ++ [ class
                    ("pointer f5 fw4 lh-title ba br2 ph3 pv2 mb2 dib "
                        ++ (case purpose of
                                Primary ->
                                    "b--transparent bg-blue hover-bg-dark-blue white"

                                Secondary ->
                                    "b--mid-gray hover-bg-moon-gray "
                                        ++ (case state of
                                                Pressed ->
                                                    "bg-light-silver"

                                                NotPressed ->
                                                    "bg-transparent"
                                           )
                           )
                    )
               ]
        )


card : String -> String -> List (Html msg) -> Html msg
card titleLeft titleRight children =
    div [ class "mw8 mv5" ]
        ([ div [ class "cf w-100 mt1" ]
            [ div [ class "fl w-90" ]
                [ h1 [ class "f5 mv0" ]
                    [ text titleLeft ]
                ]
            , div [ class "fl w-10 tr" ]
                [ h2 [ class "f5 mv0" ]
                    [ text titleRight ]
                ]
            ]
         ]
            ++ children
        )


toolbar : List (Html msg) -> Html msg
toolbar =
    div [ class "mv3" ]



-- DECODERS


noteDecoder : String -> Decoder Note
noteDecoder string =
    case Music.Note.fromString string of
        Just note ->
            Decode.succeed note

        Nothing ->
            Decode.fail "Invalid note"


qualityDecoder : String -> Decoder Quality
qualityDecoder string =
    case Music.Chord.qualityFromString string of
        Just quality ->
            Decode.succeed quality

        Nothing ->
            Decode.fail "Invalid quality"
