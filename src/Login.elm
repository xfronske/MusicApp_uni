module Login exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navbar exposing (..)



type alias Model =
    { loggedIn : Bool
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { loggedIn = False }
    , Cmd.none)



type Msg
    = GoToMain 


update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
    case msg of
        GoToMain ->
            ({ model | loggedIn = True },Cmd.none)


view : Model -> Html Msg
view _ =
     section [ class "hero is-fullheight is-default is-bold" ]
        [ div [ class "hero-head" ]
            [ nav [ class "navbar" ]
                [ div [ class "container" ]
                    [ div [ class "navbar-brand" ]
                        [ a [ class " is-rounded ",style "width" "80px", href "" ]
                            [ img [ src "./pictures/logoDesign2.png",  alt "Logo" ] []
                            ]
                        , span [ class "navbar-burger burger", attribute "data-target" "navbarMenu" ]
                            [ span [] []
                            , span [] []
                            , span [] []
                            ]
                        ]
                    , div [ id "navbarMenu", class "navbar-menu" ]
                        [ div [ class "navbar-end" ]
                            [ div [ class "tabs is-right" ]
                                [ ul []
                                    [ li [ class "is-active" ] [ a [] [ text "Home" ] ]
                                    , li [] [ a [ href "" ] [ text "Examples" ] ]
                                    , li [] [ a [ href "" ] [ text "Features" ] ]
                                    , li [] [ a [ href "" ] [ text "Team" ] ]
                                    , li [] [ a [ href "" ] [ text "Help" ] ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "hero-body" ]
            [ div [ class "container has-text-centered" ]
                [ div [ class "columns is-vcentered" ]
                    [ div [ class "column is-5" ]
                        [ figure [ class "image is-1by1" ]
                            [ img [ src "./pictures/blondGirlMusic.png", alt "Description" ] []
                            ]
                        ]
                    , div [ class "column is-6 is-offset-1" ]
                        [ h1 [ class "title is-2" ] [ text "KlangKapsel - Musik neu erleben" ]
                        , h2 [ class "subtitle is-4" ] [ text "Ihr Soundtrack auf Knopfdruck" ]
                        , br [] []
                        , p [ class "has-text-centered" ]
                            [ button [ class "button is-medium  is-success", onClick GoToMain ] [ text "Login with Spotify" ]
                            ]
                        ]
                    ]
                ]
            ]
        ,   footer [class "footer"]
        [ div [class "content has-text-centered"]
            [ p []
                [ strong [] [text "Klangkapsel"]
                , text " by "
                , a [href "https://eremiyarifat.de"] [text "Eremiya Rifat"]
                , text " and "
                , a [href "https://github.com/xfronske/MusicApp_uni/"][text "Xaver Fronske"] 
                , text ". The source code is on "
                , a [href "https://github.com/xfronske/MusicApp_uni/"] [text "GitHub"]
                , text ". The website content is made with Elm "
                ]
            ]
        ]
        ]



main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none