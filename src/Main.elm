module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import ChartCard
import Parser
import Parsers
import Samples


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    List (Result Parser.Error ChartCard.Model)


model : Model
model =
    Samples.samples
        |> List.map (Parser.run Parsers.chart >> Result.map ChartCard.init)



-- MSG


type Msg
    = ChartCardMsg Int ChartCard.Msg



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChartCardMsg msgIndex nestedMsg ->
            model
                |> List.indexedMap
                    (\index result ->
                        if index == msgIndex then
                            result |> Result.map (ChartCard.update nestedMsg)
                        else
                            result
                    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        debugBreakpoints =
            -- [ div [ class "dn-ns" ] [ text "normal" ]
            -- , div [ class "dn db-ns" ] [ text "not-small" ]
            -- , div [ class "dn db-m" ] [ text "medium" ]
            -- , div [ class "dn db-l" ] [ text "large" ]
            -- ]
            []
    in
        div [ class "flex flex-column min-vh-100" ]
            [ header [ class "tc ph4" ]
                [ h1 [ class "f3 f2-m f1-l fw2 black-90 mv3" ]
                    [ text "Open Chords Charts" ]
                , h2 [ class "f5 f4-m f3-l fw2 black-50 mt0 lh-copy" ]
                    [ text "Chart viewer and editor" ]
                ]
            , section [ class "flex-auto ph1 ph4-ns" ]
                (debugBreakpoints
                    ++ (model
                            |> List.indexedMap
                                (\index result ->
                                    case result of
                                        Ok chartCardModel ->
                                            ChartCard.view chartCardModel
                                                |> Html.map (ChartCardMsg index)

                                        Err err ->
                                            let
                                                _ =
                                                    Debug.log "Parse error" err
                                            in
                                                div []
                                                    [ p [] [ text "Chart text could not be parsed." ]
                                                    , p []
                                                        [ small []
                                                            [ text "Look at your browser developer console to see the technical error message." ]
                                                        ]
                                                    ]
                                )
                       )
                )
            , footer [ class "pa3 ph5-m ph6-l bg-near-black" ]
                [ a
                    [ class "f6 ph2 link dim moon-gray"
                    , href "https://github.com/open-chords-charts/chart-editor"
                    ]
                    [ text "Source code on GitHub" ]
                ]
            ]
