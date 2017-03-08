module ChartCard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Music.Chart exposing (..)
import Music.Chord exposing (..)
import Music.Note as Note exposing (..)
import Svg exposing (svg)
import Svg.Attributes


-- CONSTANTS


defaultChord : Chord
defaultChord =
    Chord noteC Major


defaultBar : Bar
defaultBar =
    Bar [ defaultChord ]


defaultPart : Part
defaultPart =
    Part "" [ defaultBar ]


nbBarsByRow : Int
nbBarsByRow =
    8


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
                { model
                    | chart = newChart
                    , status = newStatus
                }

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
                { model
                    | chart = newChart
                    , status = newStatus
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
                removePart partIndex model

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
        card chart.title
            (keyToString chart.key)
            [ div [ class "mv3 dt dt--fixed collapse athelas" ]
                (chart
                    |> Music.Chart.transpose viewKey
                    |> .parts
                    |> List.indexedMap (viewPart chart status)
                    |> List.concat
                )
            , toolbar
                [ label []
                    [ text "Transpose to: "
                    , viewSelectNote viewNote (Key >> SetViewKey)
                    ]
                ]
            , (case status of
                EditStatus selection ->
                    div []
                        [ button Primary
                            NotPressed
                            [ onClick Save ]
                            [ text "Save" ]
                        , (case selection of
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
                          )
                        ]

                ViewStatus ->
                    button Primary
                        NotPressed
                        [ onClick Edit ]
                        [ text "Edit" ]
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
                                            [ viewNoteSelector note
                                                (\selectedNote ->
                                                    SetChord barReference chordIndex (Chord selectedNote quality)
                                                )
                                            , viewQualitySelector quality
                                                (\selectedQuality ->
                                                    SetChord barReference chordIndex (Chord note selectedQuality)
                                                )
                                            , button Secondary
                                                NotPressed
                                                [ onClick (RemoveChord barReference chordIndex) ]
                                                [ text "Remove chord" ]
                                            ]
                                    )
                           )
                        ++ [ toolbar
                                [ button Secondary
                                    NotPressed
                                    [ onClick (AddChord barReference) ]
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
                            removeDisabled =
                                (getBarsOfPartByIndex barReference.partIndex chart |> List.length) == 1
                          in
                            button Secondary
                                NotPressed
                                [ class "mr1"
                                , disabled removeDisabled
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


viewNoteSelector : Note -> (Note -> Msg) -> Html Msg
viewNoteSelector preSelectedNote noteToMsg =
    div []
        (Note.notes
            |> List.map
                (\note ->
                    let
                        noteStr =
                            Note.toString note
                    in
                        button Secondary
                            (if note == preSelectedNote then
                                Pressed
                             else
                                NotPressed
                            )
                            [ class "mr1 w3 tc"
                            , onClick (noteToMsg note)
                            ]
                            [ text noteStr ]
                )
        )


viewQualitySelector : Quality -> (Quality -> Msg) -> Html Msg
viewQualitySelector preSelectedQuality qualityToMsg =
    div []
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
                        button Secondary
                            (if quality == preSelectedQuality then
                                Pressed
                             else
                                NotPressed
                            )
                            [ class "mr1 w4 tc"
                            , onClick (qualityToMsg quality)
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
                    ++ [ class "dtc w1 sans-serif h-inherit v-mid tc"
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
                            div [ class "dt-row h3" ]
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
                                                            in
                                                                viewBar
                                                                    status
                                                                    (isBarSelected barIndex)
                                                                    (SelectBar (BarReference partIndex barIndex))
                                                                    bar
                                                        )

                                            nbPaddingBars =
                                                nbBarsByRow - List.length nonEmptyBars

                                            emptyBar =
                                                div [ class "dtc w2" ] []

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
                                        viewBar status (isBarSelected barIndex) (SelectPart partIndex) bar
                                    )
                           )
                    )
                ]


viewBar : ChartStatus -> Bool -> Msg -> Bar -> Html Msg
viewBar status isSelected msg bar =
    div
        ([ class "dtc w2 h-inherit ba b--mid-gray v-mid f3 f2-ns"
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
            barCell s =
                div [ class "tc" ]
                    [ text s ]
          in
            case bar of
                Bar chords ->
                    case chords of
                        [ chord ] ->
                            chord
                                |> Music.Chord.toString
                                |> barCell

                        [ chord1, chord2 ] ->
                            svg
                                [ Svg.Attributes.class "h-100 w-100 v-mid"
                                , Svg.Attributes.preserveAspectRatio "none"
                                ]
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

                        _ ->
                            text "TODO"

                BarRepeat ->
                    barCell "–"
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
    div []
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


keyToString : Key -> String
keyToString (Key note) =
    Note.toString note
