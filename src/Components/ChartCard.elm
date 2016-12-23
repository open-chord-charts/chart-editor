module Components.ChartCard exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Music.Chart as Chart exposing (Chart, Key(..), Part(..))
import Music.Note as Note exposing (Note)
import Components.ChartTable as ChartTable
import Components.Types exposing (SelectedChord)


-- MODEL


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
    = ChangeTransposedKey Key
    | Edit
    | Save
    | ChartTable ChartTable.Msg



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTransposedKey key ->
            { model | viewKey = key }

        Edit ->
            let
                firstPartName =
                    List.head <|
                        List.filterMap
                            (\part ->
                                case part of
                                    Part name _ ->
                                        Just name

                                    PartRepeat _ ->
                                        Nothing
                            )
                            model.chart.parts
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

        ChartTable msg ->
            let
                model_ =
                    ChartTable.update msg
                        { chart = model.chart
                        , selectedChord = model.selectedChord
                        }
            in
                { model
                    | chart = model_.chart
                    , selectedChord = model_.selectedChord
                }



-- VIEW


view : Model -> Html Msg
view { chart, selectedChord, viewKey } =
    viewCard
        (chart.title ++ " (" ++ renderKey viewKey ++ ")")
        [ viewToolbar
            [ viewSelectKey viewKey
            , case selectedChord of
                Just _ ->
                    button [ onClick Save ] [ text "Save" ]

                Nothing ->
                    button [ onClick Edit ] [ text "Edit" ]
            ]
        , Html.map ChartTable <|
            ChartTable.view
                { chart = Chart.transpose viewKey chart
                , selectedChord = selectedChord
                }
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
    <|
        (h1 [] [ text title ])
            :: children


viewToolbar : List (Html msg) -> Html msg
viewToolbar children =
    div
        [ style
            [ ( "margin-top", "1em" )
            , ( "margin-bottom", "1em" )
            ]
        ]
    <|
        List.intersperse
            (span [ style [ ( "margin-left", "1em" ) ] ] [])
            children


viewSelectKey : Key -> Html Msg
viewSelectKey key =
    label []
        [ text "Chart key: "
        , select
            [ on "change"
                (targetValue
                    |> Json.andThen noteDecoder
                    |> Json.map (Key >> ChangeTransposedKey)
                )
            ]
          <|
            List.map (viewSelectKeyOption key) Note.notes
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



-- DECODER


noteDecoder : String -> Json.Decoder Note
noteDecoder val =
    case Note.fromString val of
        Just note ->
            Json.succeed note

        Nothing ->
            Json.fail <| "Value is not of type Note: " ++ val



-- RENDER


renderKey : Key -> String
renderKey (Key note) =
    Note.toString note
