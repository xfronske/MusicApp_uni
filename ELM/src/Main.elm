port module Main exposing (main, subscriptions)

import Browser
import Html exposing (Html, button, div, text, p, nav, a, span, i, blockquote, th, tr,li,li,section,figure,ul,h1,h2,br,footer,strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Json.Encode as Encode
import Task
import Http exposing (..)
import Html exposing (img)
import Url
import Browser.Navigation as Nav


--##########.MAIN.###########

-- ACHTUNG ACHTUNG !!!
-- Beim clonen oder online stellen muss die Navigation in der View Methode ab zeile 251 gewechselt werden ! 

    
--##########.Ports
port sendMessage: String -> Cmd msg

port messageReceiver : (String -> msg) -> Sub msg
port loginStatePort : (String -> msg) -> Sub msg
type alias Model =
    { currentPage : Int
    , dropdownState : Bool 
    , key : Nav.Key
    , url : Url.Url
    
    -- Spotify
    , spotifydDropdownState : Bool
    , accountDropdownState : Bool
    , loginState : Bool
    , token : String
    , currentUser : UserData,
      topTracks : List Track

    -- ports
    , message : String
    , accessToken: String
    , playlists : List Playlist
    }
type alias TopTracksResponse =
    { items : List Track }

type alias UserData = 
    { country : String
    , display_name : String
    , email : String
    , id : String }

type alias Playlist =
    { id : String
    , name : String
    , href : String
    }

type alias PlaylistResponse =
    { items : List Playlist }
  
type alias Track =
    {
     id : String
    , name : String,
    artists  : List Artist,
    album : Album

    }
type alias Album =
    { name : String
    , id : String
    , images : List Image
    }
type alias Image =
    { url : String
    , height : Int
    , width : Int
    }
type alias Artist =
    { name : String
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flag url key =
  ( { currentPage = 2 -- 0 = Main Page
    , dropdownState = False

    -- Spotify
    , spotifydDropdownState = False
    , key = key
    , url = url
    , accountDropdownState = False
    , loginState = False
    , token = ""
    , currentUser = { country = ""
                    , display_name = ""
                    , email = ""
                    , id = "" }
    , playlists = [],
     topTracks = []
    
    -- Flags
    
    -- Ports
    , message = ""
    , accessToken = ""

    }
  , Cmd.none
  )

--##########.Messages.and.Types.##########

type Msg
    = 
     TogglePage Int
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    -- Dropdown
    | ToggleNavigationDropdown
    | ToggleSpotifyDropdown
    | ToggleAccountDropdown
    
    -- Ports to JS
    | LogoutFromSpotify
    | RefreshToken
    | RecFromJS String 

    -- Spotify
    | ToggleLoginState Bool
    | LoadUserData
    | GotUserData (Result Http.Error UserData) 
    | ToggleUserPage
    | GotPlaylists (Result Http.Error PlaylistResponse)
    | InitiatePlaylistFetch
    | LoadUserPlaylist
    | LoadTopTracks
    | GotTopTracks (Result Http.Error TopTracksResponse)
     
    
--##########.Update.##########

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggleNavigationDropdown ->
            ( { model | dropdownState = not model.dropdownState}
            , Cmd.none )
            
        TogglePage newPage->
            ( { model | currentPage = newPage }
            , Cmd.none )

        GotPlaylists (Ok response) ->
            ( { model | playlists = response.items }, Cmd.none )

        GotPlaylists (Err _) ->
            -- hier sollten Sie die Fehlerbehandlung durchführen
            ( model, Cmd.none )

        LoadTopTracks ->
            (model, getTopTracks model)

        GotTopTracks (Ok response) ->
            ({ model | topTracks = response.items }, Cmd.none)

        GotTopTracks (Err _) ->
            (model, Cmd.none)

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
            
-- Spotify

        ToggleSpotifyDropdown ->
            ( { model | spotifydDropdownState = not model.spotifydDropdownState }
            , Cmd.none )

        ToggleAccountDropdown ->
            ( { model | accountDropdownState = not model.accountDropdownState }
            , Cmd.none )



        LogoutFromSpotify ->
            ( { model | accessToken = "" }
            , sendMessage "logout" )

        RefreshToken ->
            ( model 
            , sendMessage "refreshToken" )

        RecFromJS token ->
            ( { model | accessToken = token
                      , token = "Bearer " ++ token
                      , loginState = True }
            , Cmd.none )

        ToggleLoginState state ->
            ( { model | loginState = state }
            , Cmd.none )
        InitiatePlaylistFetch ->
            ( model, getUserPlaylists model )


-- Requests

  -- Neue Cases für die Playlist-Funktionalität
        LoadUserPlaylist ->
            (model, getUserPlaylists model)
        
        LoadUserData ->
            ( { model | currentPage = 3 } 
            , ( getUserData model) )

        GotUserData userData ->
            case userData of 
                Ok data -> 
                    ( { model | currentUser = UserData data.country data.display_name data.email data.id} 
                    , Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ToggleUserPage ->
            ( { model | currentPage = 0 }
            , Cmd.none )

       
--##########.VIEW.##########

view : Model -> Browser.Document Msg
view model =
  let
    currentPath = Url.toString model.url
  in
    case currentPath of
      "http://127.0.0.1:5500/#getPlaylists" ->
        { title = "My Playlists"
        , body = [ playlistView model ]
        }
        
      "http://127.0.0.1:5500/#getTopTracks" ->
        { title = "Top Tracks"
        , body = [ tracksView model ]
        }
      "http://127.0.0.1:5500/#getUserInfo" ->
        { title = "Profile"
        , body = [ userInfoView model ]
        }
      _ ->
        { title = "Home"
        , body = [ pageMain model ]
        }

        
--##########.PAGES.##########

pageMain : Model -> Html Msg
pageMain model = 
    div [ class "container" ][
         section [ class "hero is-fullheight is-default is-bold" ]
        [ div [ class "hero-head" ]
            [ nav [ class "navbar" ]
                [ div [ class "container" ]
                    [ div [ class "navbar-brand" ]
                        [ a [ class " is-rounded ",style "width" "80px", href "" ]
                            [ img [ src "https://eremiyarifat.de/logoDesign2.png",  alt "Logo" ] []
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
                                    , li [] [ a [onClick LoadUserPlaylist, href "#getPlaylists" ] [ text "My Playlists" ] ]
                                    , li [] [ a [onClick LoadTopTracks, href "#getTopTracks" ] [ text "Top Tracks" ] ]
                                    , li [] [ a [onClick LoadUserData, href "#getUserInfo" ] [ text "User Info" ] ]
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
                            [ img [ src "https://eremiyarifat.de/blondGirlMusic.png", alt "Description" ] []
                            ]
                        ]
                    , div [ class "column is-6 is-offset-1" ]
                        [ h1 [ class "title is-2" ] [ text "KlangKapsel - Musik neu erleben" ]
                        , h2 [ class "subtitle is-4" ] [ text "Ihr Soundtrack auf Knopfdruck" ]
                        , br [] []
                        , div [ class "column is-1 has-text-centered " ]
                            [ 
                              button [ onClick LoadUserData,class "button is-medium  is-success mt-2" ] [ text "Get User Data" ],
                              a [ onClick LoadUserPlaylist, href "#getPlaylists",class "button is-medium  is-success mt-2" ] [ text "Get Playlists" ],
                              a [ onClick LoadTopTracks, href "#getTopTracks" ,class "button is-medium  is-success mt-2" ] [ text "Get Top Tracks" ],
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
playlistView : Model -> Html Msg
playlistView model = 
    div[class "columns"][
        div[class "column is-full"][
            h1[class "column is-size-1 is-full has-text-centered	"][text "Meine Playlists"],
             div [] (List.map playlistItemView model.playlists)
        ]
    ]
playlistItemView : Playlist -> Html Msg
playlistItemView playlist =
    div [class "column is-full has-background-primary m-2 strong has-text-centered has-text-weight-bold"] [text playlist.name]      
tracksView : Model -> Html Msg
tracksView model = 
    div[class "columns"][
        div[class "column is-full"][
            h1[class "column is-size-1 is-full has-text-centered	"][text "Meine Top Tracks"],
             div [] (List.map trackItemView model.topTracks)
        ]
    ]
trackItemView : Track -> Html Msg
trackItemView track =
    div [class "column is-full  m-2 strong has-text-centered has-text-weight-bold"] [
            case List.drop 1 track.album.images |> List.head  of
            Just firstImage -> viewImage firstImage
            Nothing -> text "",
            p [] [ text (track.name ++ " - "  ) ]
            , p[] (List.map artistNames track.artists) 
    ]      

userInfoView : Model -> Html Msg
userInfoView model = 
    div[class "columns"][
        div[class "column is-full"][
            h1[class "column is-size-1 is-full has-text-centered"][text "Mein Profil"],
            div[class "column has-text-centered "][p[][strong[class "has-text-weight-bold"][text "Name : "],text model.currentUser.display_name]],
            div[class "column has-text-centered "][p[][strong[class "has-text-weight-bold"][text "Email : "],text model.currentUser.email]],
            div[class "column has-text-centered "][p[][strong[class "has-text-weight-bold"][text "Country : "],text model.currentUser.country]]
             
        ]
    ]

viewImage : Image -> Html Msg
viewImage image =
    div []
        [ img [src image.url] [] ]
artistNames : Artist -> Html Msg
artistNames artists =
    span[][text artists.name]
-- Funktion zum Erzeugen der Anzeige eines Playlist-Namens
  
    
pageSpotify : Model -> Html Msg
pageSpotify model = 
    div [ class "container for spotify" ][
          text ( "Token: " ++ model.accessToken)
        ]

pageUserAccount : Model -> Html Msg 
pageUserAccount model = 
    div [][ tr [] [ text ("email: " ++ model.currentUser.email)]
          , tr [] [ text ("display name: " ++ model.currentUser.display_name)]
          , tr [] [ text ("country:  " ++ model.currentUser.country)]
          , tr [] [ text ("Spotify Id: " ++ model.currentUser.id)]
        ]

--##########.Spotify.stuff.##########


-- Gets User Data 
getUserData : Model -> Cmd Msg  
getUserData model =
  Http.request
    { method = "GET"
    , headers = [Http.header "Authorization" model.token]
    , url = "https://api.spotify.com/v1/me"
    , body = Http.emptyBody
    , expect = Http.expectJson GotUserData decodeUserData
    , timeout = Nothing
    , tracker = Nothing
    }

getUserPlaylists : Model -> Cmd Msg  
getUserPlaylists model =

  Http.request
    { method = "GET"
    , headers = [Http.header "Authorization" model.token]
    , url = "https://api.spotify.com/v1/users/"++ model.currentUser.id ++"/playlists"
    , body = Http.emptyBody
    , expect = Http.expectJson GotPlaylists playlistResponseDecoder
    , timeout = Nothing
    , tracker = Nothing
    }
getTopTracks : Model -> Cmd Msg
getTopTracks model =
    Http.request
        { 
            method = "GET",
            headers = [ Http.header "Authorization" model.token ],
            url = "https://api.spotify.com/v1/me/top/tracks",
            body = Http.emptyBody,
            expect = Http.expectJson GotTopTracks topTracksResponseDecoder,
            timeout = Nothing,
            tracker = Nothing
        }
-- DECODER 

topTracksResponseDecoder : Decoder TopTracksResponse
topTracksResponseDecoder =
    Json.Decode.map TopTracksResponse
        (field "items" (Json.Decode.list trackDecoder))

trackDecoder : Decoder Track
trackDecoder =
    Json.Decode.map4 Track
        (field "id" string)
        (field "name" string)
        (field "artists" (Json.Decode.list artistDecoder))
        (field "album" albumDecoder)
     
artistDecoder : Decoder Artist
artistDecoder =
    Json.Decode.map Artist (field "name" string)
imageDecoder : Decoder Image
imageDecoder =
    Json.Decode.map3 Image
        (field "url" string)
        (field "height" int)
        (field "width" int)
albumDecoder : Decoder Album
albumDecoder =
    Json.Decode.map3 Album
        (field "name" string)
        (field "id" string)
        (field "images" (Json.Decode.list imageDecoder))


-- GetPlaylists Decoders
playlistDecoder : Decoder Playlist
playlistDecoder =
    Json.Decode.map3 Playlist
        (field "id" string)
        (field "name" string)
        (field "href" string)

playlistResponseDecoder : Decoder PlaylistResponse
playlistResponseDecoder =
    Json.Decode.map PlaylistResponse
        (field "items" (Json.Decode.list playlistDecoder))

decodeUserData : Decoder UserData
decodeUserData = 
    Json.Decode.map4 UserData
        (field "country" string)
        (field "display_name" string)
        (field "email" string)
        (field "id" string)




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver RecFromJS


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


{-model =
  Time.every 1000 Tick-}