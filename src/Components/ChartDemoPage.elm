module Components.ChartDemoPage exposing (..)

import Html exposing (..)
import Components.ChartCard as ChartCard
import Samples


-- MODEL


type alias Model =
    List ChartCard.Model


model : Model
model =
    [ ChartCard.init Samples.allOfMe
    , ChartCard.init Samples.allOfMe
    ]



-- MSG


type Msg
    = ChartCard Int ChartCard.Msg



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChartCard msgIndex nestedMsg ->
            List.indexedMap
                (\index item ->
                    if index == msgIndex then
                        ChartCard.update nestedMsg item
                    else
                        item
                )
                model



-- VIEW


view : Model -> Html Msg
view model =
    section [] <|
        List.indexedMap
            (\index item -> Html.map (ChartCard index) (ChartCard.view item))
            model
