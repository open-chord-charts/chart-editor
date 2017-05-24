module Views.Page exposing (frame, ActivePage(..))

{-| The frame around a typical page - that is, the header and footer.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Data.User as User exposing (User)
import Route


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type ActivePage
    = Other
    | Home


{-| Take a page's Html and frame it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
frame isLoading user page content =
    div [ class "flex flex-column min-vh-100" ]
        [ viewHeader page user isLoading
        , section [ class "flex-auto ph1 ph4-ns" ] [ content ]
        , viewFooter
        ]


viewHeader : ActivePage -> Maybe User -> Bool -> Html msg
viewHeader page user isLoading =
    header [ class "tc ph4" ]
        [ h1 [ class "f3 f2-m f1-l fw2 black-90 mv3" ]
            [ a [ Route.href (Route.Home) ] [ text "Open Chords Charts" ] ]
        , h2 [ class "f5 f4-m f3-l fw2 black-50 mt0 lh-copy" ]
            [ text "Chart viewer and editor" ]
        ]


viewFooter : Html msg
viewFooter =
    footer [ class "pa3 ph5-m ph6-l bg-near-black" ]
        [ a
            [ class "f6 ph2 link dim moon-gray"
            , href "https://github.com/open-chords-charts/chart-editor"
            ]
            [ text "Source code on GitHub" ]
        ]
