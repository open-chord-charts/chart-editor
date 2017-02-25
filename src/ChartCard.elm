module ChartCard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Music.Chart exposing (..)
import Music.Chord exposing (..)
import Music.Note as Note exposing (..)


-- CONSTANTS


defaultChord : Chord
defaultChord =
    Chord noteC Major


defaultBar : Bar
defaultBar =
    Bar [ defaultChord ]


nbBarsByRow : Int
nbBarsByRow =
    8



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
    , status : ChartStatus
    , viewKey : Key
    }


init : Chart -> Model
init chart =
    { chart = chart
    , status = ViewStatus
    , viewKey = chart.key
    }



-- MSG


type Msg
    = AddBar BarReference
    | AddChord BarReference
    | AddPart PartIndex
    | Edit
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



-- UPDATE


update : Msg -> Model -> Model
update msg model =
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
                { model
                    | chart = newChart
                    , status = newStatus
                }

        AddChord barReference ->
            let
                newChart =
                    model.chart
                        |> updateBarAt barReference (mapBarChords (\chords -> chords ++ [ defaultChord ]))
            in
                { model | chart = newChart }

        AddPart partIndex ->
            let
                newParts =
                    model.chart.parts
                        |> List.splitAt partIndex
                        |> (\( init, tail ) -> init ++ [ Part "" [ defaultBar ] ] ++ tail)

                chart =
                    model.chart

                newChart =
                    { chart | parts = newParts }

                newStatus =
                    EditStatus (PartSelection partIndex)
            in
                { model
                    | chart = newChart
                    , status = newStatus
                }

        Edit ->
            let
                newStatus =
                    EditStatus (BarSelection { partIndex = 0, barIndex = 0 })
            in
                { model | status = newStatus }

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
                { model
                    | chart = newChart
                    , status = newStatus
                }

        RemoveChord barReference chordIndex ->
            let
                newChart =
                    model.chart
                        |> updateBarAt barReference (mapBarChords (List.removeAt chordIndex))
            in
                { model | chart = newChart }

        RemovePart partIndex ->
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
                { model
                    | chart = newChart
                    , status = newStatus
                }

        Save ->
            { model | status = ViewStatus }

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
                { model | chart = newChart }

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
                { model | chart = newChart }

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
                { model | chart = newChart }

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
                { model | chart = newChart }

        SetViewKey key ->
            { model | viewKey = key }



-- VIEW


view : Model -> Html Msg
view { chart, status, viewKey } =
    let
        (Key viewNote) =
            viewKey
    in
        div []
            [ h1 []
                [ text (chart.title ++ " (" ++ keyToString chart.key ++ ")")
                ]
            , label []
                [ text "Transpose to: "
                , viewSelectNote viewNote (Key >> SetViewKey)
                ]
            , table
                [ style
                    [ ( "border-collapse", "collapse" )
                    , ( "width", "400px" )
                    ]
                ]
                [ let
                    transposedChart =
                        Music.Chart.transpose viewKey chart
                  in
                    tbody []
                        (transposedChart.parts
                            |> List.indexedMap (viewPart chart status)
                        )
                ]
            , div []
                (case status of
                    EditStatus selection ->
                        [ button [ onClick Save ]
                            [ text "Save" ]
                        , fieldset []
                            (case selection of
                                BarSelection barReference ->
                                    [ legend []
                                        [ text "Bar" ]
                                    ]
                                        ++ (case getBarAtReference barReference chart of
                                                Nothing ->
                                                    []

                                                Just selectedBar ->
                                                    [ viewBarEditor chart barReference selectedBar ]
                                           )

                                PartSelection partIndex ->
                                    [ legend []
                                        [ text "Part" ]
                                    ]
                                        ++ (case List.getAt partIndex chart.parts of
                                                Nothing ->
                                                    []

                                                Just part ->
                                                    let
                                                        removeDisabled =
                                                            List.length chart.parts == 1
                                                    in
                                                        [ viewPartEditor removeDisabled partIndex part ]
                                           )
                            )
                        ]

                    ViewStatus ->
                        [ button [ onClick Edit ]
                            [ text "Edit" ]
                        ]
                )
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
                , text "repeated bar"
                ]
    in
        div []
            ((case bar of
                Bar chords ->
                    [ barRepeatCheckbox False
                    , br [] []
                    ]
                        ++ (chords
                                |> List.indexedMap
                                    (\chordIndex (Chord note quality) ->
                                        [ viewSelectNote note
                                            (\selectedNote ->
                                                SetChord barReference chordIndex (Chord selectedNote quality)
                                            )
                                        , viewSelectQuality quality
                                            (\selectedQuality ->
                                                SetChord barReference chordIndex (Chord note selectedQuality)
                                            )
                                        , button [ onClick (RemoveChord barReference chordIndex) ]
                                            [ text "Remove chord" ]
                                        , br [] []
                                        ]
                                    )
                                |> List.concat
                           )
                        ++ [ button [ onClick (AddChord barReference) ]
                                [ text "Add chord in bar" ]
                           ]

                BarRepeat ->
                    [ barRepeatCheckbox True ]
             )
                ++ [ br [] []
                   , button [ onClick (AddBar barReference) ]
                        [ text "Add bar before" ]
                   , button [ onClick (AddBar { barReference | barIndex = barReference.barIndex + 1 }) ]
                        [ text "Add bar after" ]
                   , let
                        removeDisabled =
                            (getBarsOfPartByIndex barReference.partIndex chart |> List.length) == 1
                     in
                        button
                            [ disabled removeDisabled
                            , onClick (RemoveBar barReference)
                            ]
                            [ text "Remove bar" ]
                   , br [] []
                   , button [ onClick (SelectPart barReference.partIndex) ]
                        [ text "Select part" ]
                   ]
            )


viewPartEditor : Bool -> PartIndex -> Part -> Html Msg
viewPartEditor removeDisabled partIndex part =
    let
        partRepeatCheckbox isChecked =
            label []
                [ input
                    [ checked isChecked
                    , onCheck (SetPartRepeat partIndex)
                    , type_ "checkbox"
                    ]
                    []
                , text "repeated part"
                ]

        isRepeat =
            isPartRepeat part
    in
        div []
            ([ partRepeatCheckbox isRepeat
             , br [] []
             , input
                [ onInput (SetPartName partIndex)
                , value (getPartName part)
                ]
                []
             ]
                ++ (if isRepeat then
                        []
                    else
                        [ br [] []
                        , button []
                            [ text "Add bar at start" ]
                        , button []
                            [ text "Add bar at end" ]
                        ]
                   )
                ++ [ br [] []
                   , button [ onClick (AddPart partIndex) ]
                        [ text "Add part before" ]
                   , button [ onClick (AddPart (partIndex + 1)) ]
                        [ text "Add part after" ]
                   , button []
                        [ text "Duplicate part" ]
                   , button
                        [ disabled removeDisabled
                        , onClick (RemovePart partIndex)
                        ]
                        [ text "Remove part" ]
                   ]
            )


viewSelectNote : Note -> (Note -> Msg) -> Html Msg
viewSelectNote selectedNote tagger =
    select
        [ on "change"
            (targetValue
                |> Decode.andThen noteDecoder
                |> Decode.map tagger
            )
        ]
        (Note.notes
            |> List.map
                (\note ->
                    let
                        noteStr =
                            Note.toString note
                    in
                        option
                            [ selected (note == selectedNote)
                            , value noteStr
                            ]
                            [ text noteStr ]
                )
        )


viewSelectQuality : Quality -> (Quality -> Msg) -> Html Msg
viewSelectQuality selectedQuality tagger =
    select
        [ on "change"
            (targetValue
                |> Decode.andThen qualityDecoder
                |> Decode.map tagger
            )
        ]
        (Music.Chord.qualities
            |> List.map
                (\quality ->
                    let
                        qualityStr =
                            case quality of
                                Major ->
                                    "Major"

                                Minor ->
                                    "minor"

                                Seventh ->
                                    "7th"
                    in
                        option
                            [ selected (quality == selectedQuality)
                            , value qualityStr
                            ]
                            [ text qualityStr ]
                )
        )


viewPart : Chart -> ChartStatus -> PartIndex -> Part -> Html Msg
viewPart chart status partIndex part =
    tr
        [ style
            (if isPartRepeat part then
                []
             else
                [ ( "height", "2em" ) ]
            )
        ]
        (let
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

            partTd s =
                td
                    [ onClick (SelectPart partIndex)
                    , style
                        ([ ( "width", "1em" ) ]
                            ++ (if isPartSelected then
                                    [ ( "background-color", "lightgray" ) ]
                                else
                                    []
                               )
                        )
                    ]
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
                    partTd partName
                        :: (List.greedyGroupsOf nbBarsByRow bars
                                |> List.concat
                                |> List.indexedMap
                                    (\barIndex bar ->
                                        viewBar (isBarSelected barIndex) (SelectBar (BarReference partIndex barIndex)) bar
                                    )
                           )

                PartRepeat partName ->
                    partTd partName
                        :: (List.repeat nbBarsByRow BarRepeat
                                |> List.indexedMap
                                    (\barIndex bar ->
                                        viewBar (isBarSelected barIndex) (SelectPart partIndex) bar
                                    )
                           )
        )


viewBar : Bool -> Msg -> Bar -> Html Msg
viewBar isSelected msg bar =
    td
        [ onClick msg
        , style
            ([ ( "border", "1px solid" )
             , ( "height", "inherit" )
             , ( "padding", "0" )
             , ( "text-align", "center" )
             , ( "vertical-align", "middle" )
             , ( "width", "2.5em" )
             ]
                ++ (if isSelected then
                        [ ( "background-color", "lightgray" ) ]
                    else
                        []
                   )
            )
        ]
        [ text (barToString bar) ]



-- DECODERS


noteDecoder : String -> Decoder Note
noteDecoder string =
    case Note.fromString string of
        Just note ->
            Decode.succeed note

        Nothing ->
            Decode.fail string


qualityDecoder : String -> Decoder Quality
qualityDecoder string =
    case string of
        "Major" ->
            Decode.succeed Major

        "minor" ->
            Decode.succeed Minor

        "7th" ->
            Decode.succeed Seventh

        _ ->
            Decode.fail string



-- FORMATTERS


barToString : Bar -> String
barToString bar =
    case bar of
        Bar chords ->
            chords
                |> List.map Music.Chord.toString
                |> String.join ", "

        BarRepeat ->
            "–"


keyToString : Key -> String
keyToString (Key note) =
    Note.toString note
