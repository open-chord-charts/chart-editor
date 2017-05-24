module Main exposing (main)

import Html exposing (..)
import Navigation exposing (Location)
import Data.Session exposing (Session)
import Route exposing (..)
import Page.Home as Home
import Page.ChartCard as ChartCard
import Page.NotFound as NotFound
import Views.Page as Page


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



-- MODEL --


type Page
    = Blank
    | NotFound
    | Home Home.Model
    | Chart (Result ChartCard.LoadError ChartCard.Model)


type PageState
    = Loaded Page
    | TransitioningFrom Page


type alias Model =
    { pageState : PageState
    , session : Session
    }


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialPage
        , session = { user = Nothing }
        }


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session.user
    in
        case page of
            NotFound ->
                NotFound.view session
                    |> frame Page.Other

            Blank ->
                -- This is for the very intiial page load, while we are loading
                -- data via HTTP. We could also render a spinner here.
                Html.text ""
                    |> frame Page.Other

            Home subModel ->
                Home.view session subModel
                    |> frame Page.Home

            Chart (Err err) ->
                Html.text ("Error: " ++ Basics.toString err)
                    |> frame Page.Other

            Chart (Ok subModel) ->
                ChartCard.view subModel
                    |> frame Page.Other
                    |> Html.map ChartCardMsg


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | ChartCardMsg ChartCard.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } ! []

        Just (Route.Home) ->
            { model | pageState = Loaded (Home Home.initialModel) } ! []

        Just (Route.Chart slug) ->
            { model | pageState = Loaded (Chart (ChartCard.initialModel slug)) } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( ChartCardMsg subMsg, Chart (Ok subModel) ) ->
                let
                    newSubModel =
                        ChartCard.update subMsg subModel
                in
                    { model | pageState = newSubModel |> Ok |> Chart |> Loaded } ! []

            ( _, NotFound ) ->
                -- Disregard incoming messages when we're on the
                -- NotFound page.
                model ! []

            ( _, _ ) ->
                -- Disregard incoming messages that arrived for the wrong page
                model ! []
