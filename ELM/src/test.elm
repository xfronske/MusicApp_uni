module Main exposing (..)

-- Importierte Module
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url

-- Hauptprogramm
-- Dies definiert, wie die Anwendung initialisiert wird und wie sie auf 
-- verschiedene Arten von Ereignissen reagiert
main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- Modell
-- Dies definiert die Datenstruktur, die den Zustand der Anwendung enthält
type alias Model =
  { key : Nav.Key
  , url : Url.Url
  }

-- Init-Funktion
-- Diese Funktion wird aufgerufen, wenn die Anwendung zum ersten Mal gestartet wird
init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url, Cmd.none )

-- Nachrichtentypen
-- Diese definieren die verschiedenen Arten von Ereignissen, die in der Anwendung auftreten können
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

-- Update-Funktion
-- Diese Funktion wird aufgerufen, wenn ein Ereignis eintritt
-- Es erhält das Ereignis und den aktuellen Zustand der Anwendung und gibt den 
-- neuen Zustand und eventuelle Nebeneffekte zurück
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

-- Abonnements
-- Diese Funktion gibt eine Liste von Ereignisquellen zurück, die die Anwendung 
-- abonnieren sollte
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- Ansicht
-- Diese Funktion wird aufgerufen, um das HTML zu generieren, das auf der 
-- Seite angezeigt werden soll. Es erhält den aktuellen Zustand der Anwendung
view : Model -> Browser.Document Msg
view model =
  let
    currentPath = Url.toString model.url
  in
    case currentPath of
      "http://localhost:8000/home" ->
        { title = "Home"
        , body = [ text "Willkommen auf der Startseite!" ]
        }
        
      "http://localhost:8000/profile" ->
        { title = "Profile"
        , body = [ text "Das ist die Profilseite!" ]
        }

      "/reviews/the-century-of-the-self" ->
        { title = "The Century of the Self"
        , body = [ text "Das ist eine Rezensionsseite für 'The Century of the Self'!" ]
        }

      "/reviews/public-opinion" ->
        { title = "Public Opinion"
        , body = [ text "Das ist eine Rezensionsseite für 'Public Opinion'!" ]
        }

      "/reviews/shah-of-shahs" ->
        { title = "Shah of Shahs"
        , body = [ text "Das ist eine Rezensionsseite für 'Shah of Shahs'!" ]
        }

      _ ->
        { title = "404 Not Found"
        , body = [ text "Die angeforderte Seite konnte nicht gefunden werden." 
        ,p[][text currentPath]
        ,viewLink "/home"
        ,viewLink "/profile"
        
        ]
        }

-- Hilfsfunktion, um einen Link zu erzeugen
viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]


class "container" ][
          text ("Main Page"++model.accessToken),
          p [][text ("User Data : " ++ model.currentUser.display_name)],
          button [onClick LoadUserData][text "getuserData"],
          button [onClick LoadUserPlaylist][text "getuserPlaylist"],
          p [][text ("Playlist: ")],
          div [] (List.map playlistNameView model.playlists),
          button [ onClick LoadTopTracks ] [ text "Top-Tracks laden" ],
            div []  (List.map trackView model.topTracks)

            -- li,section,figure,ul,h1,br,footer,strong





pageMain : Model -> Html Msg
pageMain model = 
    div [ class "container" ][
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
                                    , li [] [ a [ href "#getPlaylists" ] [ text "My Playlists" ] ]
                                    , li [] [ a [ href "#getTopTracks" ] [ text "Top Tracks" ] ]
                                    , li [] [ a [ href "#getUserInfo" ] [ text "User Info" ] ]
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
                        , div [ class "column is-1 has-text-centered " ]
                            [ button [ onClick LoadUserPlaylist, class "button is-medium  is-success mt-2" ] [ text "Load Playlists" ],
                             a [ href "#getPlaylists",class "button is-medium  is-success mt-2" ] [ text "Get Playlists" ],
                              a [ href "#getTopTracks" ,class "button is-medium  is-success mt-2" ] [ text "Get Top Tracks" ],
                              a [ href "#getUserInfo" ,class "button is-medium  is-success mt-2" ] [ text "get Profile Information" ]
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
                , text " and Xaver Fronske" 
                , text ". The source code is on "
                , a [href "https://github.com/xfronske/MusicApp_uni/"] [text "GitHub"]
                , text ". The website content is made with Elm "
                ]
            ]
        ]
    ]
    ]