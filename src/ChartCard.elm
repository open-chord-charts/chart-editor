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


type alias Row =
    Part


partToRows : Part -> List Row
partToRows part =
    case part of
        Part partName bars ->
            List.greedyGroupsOf nbBarsByRow bars
                |> List.map (Part partName)

        PartRepeat partName ->
            [ PartRepeat partName ]


rowToBars : Row -> List Bar
rowToBars row =
    case row of
        Part _ bars ->
            bars

        PartRepeat _ ->
            List.repeat nbBarsByRow BarRepeat



-- MODEL


type alias BarReference =
    { partName : String
    , barIndex : Int
    }


updateChartAt : BarReference -> (Bar -> Bar) -> Chart -> Chart
updateChartAt barReference updateBar chart =
    let
        newParts =
            chart.parts
                |> List.map
                    (\part ->
                        case part of
                            Part partName bars ->
                                if partName == barReference.partName then
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
    | EditStatus BarReference


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
    | SetBarRepeat BarReference Bool
    | SetChord BarReference Chord
    | SetViewKey Key



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Edit ->
            let
                firstPartName =
                    model.chart.parts
                        |> List.filterMap
                            (\part ->
                                case part of
                                    Part name _ ->
                                        Just name

                                    PartRepeat _ ->
                                        Nothing
                            )
                        |> List.head

                newStatus =
                    case firstPartName of
                        Just name ->
                            EditStatus { partName = name, barIndex = 0 }

                        Nothing ->
                            ViewStatus
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
                            EditStatus barReference
            in
                { model | status = newStatus }

        SetBarRepeat barReference checked ->
            let
                newChart =
                    model.chart
                        |> updateChartAt barReference
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
                        |> updateChartAt barReference (\_ -> Bar [ chord ])
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
        viewCard
            (chart.title ++ " (" ++ keyToString chart.key ++ ")")
            [ div [ style [ ( "margin", "1em 0" ) ] ]
                [ label []
                    [ text "Transpose to: "
                    , viewSelectNote viewNote (Key >> SetViewKey)
                    ]
                ]
            , viewTable (Music.Chart.transpose viewKey chart) status
            , viewToolbar
                (case status of
                    EditStatus barReference ->
                        [ button [ onClick Save ] [ text "Save" ] ]
                            ++ (let
                                    selectedBar =
                                        chart
                                            |> getBarsOfPart barReference.partName
                                            |> List.getAt barReference.barIndex
                                in
                                    case selectedBar of
                                        Nothing ->
                                            []

                                        Just selectedBar ->
                                            [ viewBarEditor barReference selectedBar ]
                               )

                    ViewStatus ->
                        [ button [ onClick Edit ] [ text "Edit" ] ]
                )
            ]


viewCard : String -> List (Html a) -> Html a
viewCard title children =
    article
        [ style
            [ ( "border", "1px solid" )
            , ( "margin", "1em" )
            , ( "padding", "1em" )
            , ( "width", "500px" )
            ]
        ]
        ((h1 [] [ text title ])
            :: children
        )


viewToolbar : List (Html msg) -> Html msg
viewToolbar children =
    div
        [ style
            [ ( "margin-top", "1em" )
            , ( "margin-bottom", "1em" )
            ]
        ]
        (List.intersperse
            (span [ style [ ( "margin-left", "1em" ) ] ] [])
            children
        )


viewBarEditor : BarReference -> Bar -> Html Msg
viewBarEditor barReference bar =
    case bar of
        Bar chords ->
            span []
                (chords
                    |> List.map
                        (\(Chord note quality) ->
                            viewSelectNote note
                                (\selectedNote ->
                                    SetChord barReference (Chord selectedNote quality)
                                )
                        )
                )

        BarRepeat ->
            label []
                [ input
                    [ checked True
                    , onCheck (SetBarRepeat barReference)
                    , type_ "checkbox"
                    ]
                    []
                , text "repeated bar"
                ]


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


viewTable : Chart -> ChartStatus -> Html Msg
viewTable chart chartStatus =
    table
        [ style
            [ ( "border-collapse", "collapse" )
            , ( "width", "400px" )
            ]
        ]
        [ tbody []
            (chart.parts
                |> List.concatMap partToRows
                |> List.map (viewRow chartStatus)
            )
        ]


viewRow : ChartStatus -> Row -> Html Msg
viewRow chartStatus row =
    let
        partName =
            getPartName row
    in
        tr
            [ style
                (case row of
                    Part _ _ ->
                        [ ( "height", "2em" ) ]

                    PartRepeat _ ->
                        []
                )
            ]
            ((td
                [ style [ ( "width", "1em" ) ] ]
                [ text partName ]
             )
                :: (row
                        |> rowToBars
                        |> List.indexedMap
                            (\barIndex bar ->
                                let
                                    isSelected =
                                        case chartStatus of
                                            EditStatus barReference ->
                                                partName == barReference.partName && barIndex == barReference.barIndex

                                            ViewStatus ->
                                                False
                                in
                                    viewBar isSelected (BarReference partName barIndex) bar
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
noteDecoder val =
    case Note.fromString val of
        Just note ->
            Decode.succeed note

        Nothing ->
            Decode.fail ("Value is not of type Note: " ++ val)



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
