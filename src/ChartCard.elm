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


nbBarsByRow : Int
nbBarsByRow =
    8



-- TYPES


type alias BarIndex =
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


isSamePartNameThan : PartName -> PartIndex -> Chart -> Bool
isSamePartNameThan partName partIndex chart =
    case getPartNameFromIndex partIndex chart of
        Nothing ->
            False

        Just partNameFromIndex ->
            partName == partNameFromIndex


getBarAtReference : BarReference -> Chart -> Maybe Bar
getBarAtReference barReference chart =
    chart.parts
        |> List.getAt barReference.partIndex
        |> Maybe.andThen
            (\part ->
                getBarsOfPart (getPartName part) chart
                    |> List.getAt barReference.barIndex
            )


updateBarAt : BarReference -> (Bar -> Bar) -> Chart -> Chart
updateBarAt barReference updateBar chart =
    let
        newParts =
            chart.parts
                |> List.map
                    (\part ->
                        case part of
                            Part partName bars ->
                                if isSamePartNameThan partName barReference.partIndex chart then
                                    let
                                        newBars =
                                            bars
                                                |> List.indexedMap
                                                    (\index bar ->
                                                        if index == barReference.barIndex then
                                                            updateBar bar
                                                        else
                                                            bar
                                                    )
                                    in
                                        Part partName newBars
                                else
                                    part

                            PartRepeat _ ->
                                part
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
    = Edit
    | Save
    | SelectBar BarReference
    | SelectPart PartIndex
    | SetBarRepeat BarReference Bool
    | SetChord BarReference Chord
    | SetViewKey Key



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Edit ->
            let
                newStatus =
                    EditStatus (BarSelection { partIndex = 0, barIndex = 0 })
            in
                { model | status = newStatus }

        Save ->
            { model | status = ViewStatus }

        SelectBar barReference ->
            let
                newStatus =
                    case model.status of
                        ViewStatus ->
                            ViewStatus

                        EditStatus _ ->
                            EditStatus (BarSelection barReference)
            in
                { model | status = newStatus }

        SelectPart partIndex ->
            let
                newStatus =
                    case model.status of
                        ViewStatus ->
                            ViewStatus

                        EditStatus _ ->
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
                                    Bar []
                            )
            in
                { model | chart = newChart }

        SetChord barReference chord ->
            let
                newChart =
                    model.chart
                        |> updateBarAt barReference (\_ -> Bar [ chord ])
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
                                                    [ viewBarEditor barReference selectedBar ]
                                           )

                                PartSelection partIndex ->
                                    let
                                        partName =
                                            getPartNameFromIndex partIndex chart
                                                |> Maybe.withDefault "Should never happen"
                                    in
                                        [ legend []
                                            [ text ("Part " ++ partName) ]
                                        , button []
                                            [ text "Add part before" ]
                                        , button []
                                            [ text "Add part after" ]
                                        , button []
                                            [ text "Delete part" ]
                                        ]
                            )
                        ]

                    ViewStatus ->
                        [ button [ onClick Edit ]
                            [ text "Edit" ]
                        ]
                )
            ]


viewBarEditor : BarReference -> Bar -> Html Msg
viewBarEditor barReference bar =
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
                                |> List.concatMap
                                    (\(Chord note quality) ->
                                        [ viewSelectNote note
                                            (\selectedNote -> SetChord barReference (Chord selectedNote quality))
                                        , viewSelectQuality quality
                                            (\selectedQuality -> SetChord barReference (Chord note selectedQuality))
                                        , button []
                                            [ text "Delete chord" ]
                                        ]
                                    )
                           )
                        ++ [ br [] []
                           , button []
                                [ text "Add chord in bar" ]
                           ]

                BarRepeat ->
                    [ barRepeatCheckbox True ]
             )
                ++ [ br [] []
                   , button []
                        [ text "Add bar before" ]
                   , button []
                        [ text "Add bar after" ]
                   , button []
                        [ text "Delete bar" ]
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
                            Basics.toString quality
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
            partTd s =
                td
                    [ onClick (SelectPart partIndex)
                    , style [ ( "width", "1em" ) ]
                    ]
                    [ text s ]

            isSelected : PartName -> BarIndex -> Bool
            isSelected partName barIndex =
                case status of
                    EditStatus selection ->
                        case selection of
                            BarSelection barReference ->
                                isSamePartNameThan partName barReference.partIndex chart && barIndex == barReference.barIndex

                            PartSelection partIndex ->
                                isSamePartNameThan partName partIndex chart

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
                                        viewBar (isSelected partName barIndex) (BarReference partIndex barIndex) bar
                                    )
                           )

                PartRepeat partName ->
                    partTd partName
                        :: (List.repeat nbBarsByRow BarRepeat
                                |> List.indexedMap
                                    (\barIndex bar ->
                                        viewBar (isSelected partName barIndex) (BarReference partIndex barIndex) bar
                                    )
                           )
        )


viewBar : Bool -> BarReference -> Bar -> Html Msg
viewBar isSelected barReference bar =
    td
        [ onClick (SelectBar barReference)
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

        "Minor" ->
            Decode.succeed Minor

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
