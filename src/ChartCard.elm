module ChartCard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Music.Chart exposing (..)
import Music.Chord exposing (..)
import Music.Note as Note exposing (..)


-- TYPES


type alias BarIndex =
    Int


type alias RowIndex =
    Int


type alias Row =
    { partName : Maybe PartName
    , isFromRepeatPart : Bool
    , bars : List Bar
    }



-- MODEL


type alias SelectedChord =
    { partName : String
    , barIndex : Int
    }


type alias Model =
    { chart : Chart
    , selectedChord : Maybe SelectedChord
    , viewKey : Key
    }


init : Chart -> Model
init chart =
    { chart = chart
    , selectedChord = Nothing
    , viewKey = chart.key
    }



-- MSG


type Msg
    = Edit
    | Save
    | SetViewKey Key
    | SelectChord RowIndex BarIndex



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetViewKey key ->
            { model | viewKey = key }

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
            in
                { model
                    | selectedChord =
                        case firstPartName of
                            Just name ->
                                Just { partName = name, barIndex = 0 }

                            Nothing ->
                                Nothing
                }

        Save ->
            { model | selectedChord = Nothing }

        SelectChord rowIndex barIndex ->
            let
                newSelectedChord =
                    model.selectedChord
                        |> Maybe.map (\selectedChord -> { selectedChord | barIndex = barIndex, partName = "A" })
            in
                { model | selectedChord = newSelectedChord }



-- VIEW


view : Model -> Html Msg
view { chart, selectedChord, viewKey } =
    viewCard
        (chart.title ++ " (" ++ formatKey viewKey ++ ")")
        [ viewToolbar
            [ viewSelectKey viewKey
            , case selectedChord of
                Just _ ->
                    button [ onClick Save ] [ text "Save" ]

                Nothing ->
                    button [ onClick Edit ] [ text "Edit" ]
            ]
        , viewTable
            (Music.Chart.transpose viewKey chart)
            selectedChord
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


viewSelectKey : Key -> Html Msg
viewSelectKey key =
    label []
        [ text "Chart key: "
        , select
            [ on "change"
                (targetValue
                    |> Decode.andThen noteDecoder
                    |> Decode.map (Key >> SetViewKey)
                )
            ]
            (List.map (viewSelectKeyOption key) Note.notes)
        ]


viewSelectKeyOption : Key -> Note -> Html Msg
viewSelectKeyOption key note =
    let
        noteStr =
            Note.toString note
    in
        option
            [ selected ((Key note) == key)
            , value noteStr
            ]
            [ text noteStr ]


viewTable : Chart -> Maybe SelectedChord -> Html Msg
viewTable chart selectedChord =
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
        ((td
            [ style [ ( "width", "1em" ) ] ]
            [ text (Maybe.withDefault "" partName) ]
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
        )


viewBar : Bool -> Int -> Int -> Bar -> Html Msg
viewBar selected rowIndex barIndex bar =
    td
        [ onClick (SelectChord rowIndex barIndex)
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
        [ text (formatBar bar) ]



-- DECODERS


noteDecoder : String -> Decoder Note
noteDecoder val =
    case Note.fromString val of
        Just note ->
            Decode.succeed note

        Nothing ->
            Decode.fail ("Value is not of type Note: " ++ val)



-- FORMATTERS


formatBar : Bar -> String
formatBar bar =
    case bar of
        Bar ( a, b, c, d ) ->
            Music.Chord.toString a

        BarRepeat ->
            "–"


formatKey : Key -> String
formatKey (Key note) =
    Note.toString note



-- HELPERS


toRows : Part -> List Row
toRows part =
    case part of
        Part name bars ->
            let
                barGroups =
                    List.greedyGroupsOf 8 bars
            in
                List.indexedMap
                    (\index barGroup ->
                        Row
                            (if index == 0 then
                                Just name
                             else
                                Nothing
                            )
                            False
                            barGroup
                    )
                    barGroups

        PartRepeat name ->
            [ Row (Just name) True (List.repeat 8 BarRepeat) ]
