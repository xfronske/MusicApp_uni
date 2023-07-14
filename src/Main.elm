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
import Svg exposing (..)
import Svg.Attributes as SVG exposing (x, y, width, height, fill)
import Debug exposing (toString)
import Dict exposing (Dict)




--##########.MAIN.###########

-- ACHTUNG ACHTUNG !!!
-- Beim clonen oder online stellen muss die Navigation in der View Methode ab zeile 251 gewechselt werden ! 
-- Damit die Funktionen richtig funktionieren muss zuerst der Button "Get User Data" aufgerufen werden
-- beim clonen auf die SpotifyAPI client_id und redirect uri achten
-- dieser Coder verwerden : http://127.0.0.1:5500/ als uri 

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked ----- TODO app.ports sub batch und dann nachricht ausgeben von player
    }

    
--##########.Ports.##########

port sendMessage : String -> Cmd msg

port messageReceiver : (String -> msg) -> Sub msg

--##########.Messages.and.Types.##########

type Msg 
    = TogglePage Int

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
    | GetUserData
    | GotUserData (Result Http.Error UserData) 
    | ToggleUserPage
    | GotPlaylists (Result Http.Error PlaylistResponse)
    | GetPlaylists
    | GetUserPlaylist
    | GetTopTracks
    | GotTopTracks (Result Http.Error TopTracksResponse)

    -- Playback
    | IncVolume
    | DecVolume
    | TogglePlay 
    | NextTrack
    | PrevTrack

    -- URL-Nav
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

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
    , currentUser : UserData
    , topTracks : List Track

    -- Playback
    , volume : Int
    , playbackState : Bool

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
    , href : String }

type alias PlaylistResponse =
    { items : List Playlist }
  
type alias Track =
    { id : String
    , name : String
    , artists  : List Artist
    , album : Album 
    , popularity : Int }

type alias Album =
    { name : String
    , id : String
    , images : List Image }

type alias Image =
    { url : String
    , height : Int
    , width : Int }

type alias Artist =
    { name : String }

--##########.Init.##########

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flag url key =
  ( { currentPage = 2 -- 0 = Main Page
    , dropdownState = False

    -- Spotify
    , spotifydDropdownState = False
    , accountDropdownState = False
    , loginState = False
    , token = ""
    , currentUser = { country = ""
                    , display_name = ""
                    , email = ""
                    , id = "" }
    , playlists = []
    , topTracks = []

    -- Playback
    , volume = 5
    , playbackState = False
    
    -- Ports
    , message = ""
    , accessToken = ""

    -- URL-Navigation
    , key = key
    , url = url

    }
  , Cmd.none )

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

-- Requests

        GetUserPlaylist ->
            ( model, getUserPlaylists model )
        
        GetUserData ->
            ( { model | currentPage = 3 } 
            , ( getUserData model) )

        GotUserData userData ->
            case userData of 
                Ok data -> 
                    ( { model | currentUser = UserData data.country data.display_name data.email data.id } 
                    , Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ToggleUserPage ->
            ( { model | currentPage = 0 }
            , Cmd.none )

        GetPlaylists ->
            ( model, getUserPlaylists model )

        GotPlaylists response ->
            case response of 
                Ok data ->
                    ( { model | playlists = data.items }
                    , Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetTopTracks ->
            (model, getTopTracks model)

        GotTopTracks response ->
            case response of 
                Ok data ->
                    ( { model | topTracks = data.items }
                    , Cmd.none)

                Err _ ->
                     ( model, Cmd.none )

-- Playback

        IncVolume ->
            if model.volume < 10 then 
                ( { model | volume = model.volume + 1 }
                , sendMessage "inc_vol" ) 
            else ( model , Cmd.none )

        DecVolume -> 
            if model.volume > 0 then 
                ( { model | volume = model.volume - 1 }
                , sendMessage "dec_vol" ) 
            else ( model , Cmd.none )

        TogglePlay ->
            ( { model | playbackState = not model.playbackState } 
            , sendMessage "toggle_play" )

        NextTrack ->
            ( model, sendMessage "next_track" )

        PrevTrack ->
            ( model, sendMessage"prev_track" )

-- URL-Nav
        LinkClicked urlRequest ->
             case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model
                    , Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none )

--##########.Histogram.##########

popularityHistogram : List Track -> Dict Int Int
popularityHistogram tracks =
    List.foldl (\track acc -> 
                    let 
                        popularity = track.popularity
                        count = Maybe.withDefault 0 (Dict.get popularity acc)
                    in 
                        Dict.insert popularity (count + 1) acc
                ) Dict.empty tracks

displayHistogram : Dict Int Int -> Html msg
displayHistogram histogram =
    let
        toBar (popularity, count) =
            div [] [ Html.text (String.fromInt popularity ++ ": " ++ String.fromInt count) ]

        sortedHistogram =
            List.sortBy Tuple.first (Dict.toList histogram)
    in
    div [] (List.map toBar sortedHistogram)

histogramToSvg : Dict.Dict Int Int -> Svg.Svg Msg
histogramToSvg histogram =
    let
        dictList = Dict.toList histogram
        maxCount = List.maximum (List.map Tuple.second dictList) |> Maybe.withDefault 0
    in
    Svg.svg [ SVG.width "100%", SVG.height "100%", SVG.viewBox "0 0 100 100" ] 
        (List.indexedMap (\i (popularity, count) -> 
            Svg.rect 
            [ SVG.x <| String.fromFloat (toFloat i ) 
            , SVG.y <| String.fromFloat (toFloat (100 - (count * 10 // maxCount))) 
            , SVG.width "1"
            , SVG.height <| String.fromFloat (toFloat (count * 10 // maxCount))
            , SVG.fill "black"
            ] 
            []
        ) dictList)


--##########.VIEW.##########

view : Model -> Browser.Document Msg
view model =
  let
    currentPath = Url.toString model.url
  in
    case currentPath of
      "Https://xfronske.github.io/#getTopPlaylists" ->
        { title = "My Playlists"
        , body = [ playlistView model ] }
        
      "Https://xfronske.github.io/#getTopTracks" ->
        { title = "Top Tracks"
        , body = [ viewTracks model ] }

      "Https://xfronske.github.io/#getUserInfo" ->
        { title = "Profile"
        , body = [ viewUserInfo model ] }

      "Https://xfronske.github.io/#errorPage" ->
        { title = "Profile"
        , body = [ Html.text "Hallo Error" ] }

      "Https://xfronske.github.io/#musicPlayer" ->
        { title = "Music via Spotify"
        , body = [ pageMusic model] }

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
                        [ Html.a [ class " is-rounded ",Html.Attributes.style "width" "80px", href "" ]
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
                                    [ li [ class "is-active" ] [ Html.a [] [ Html.text "Home" ] ]
                                    , li [] [ Html.a [onClick GetUserPlaylist, href "#getPlaylists" ] [ Html.text "My Playlists" ] ]
                                    , li [] [ Html.a [onClick GetTopTracks, href "#getTopTracks" ] [ Html.text "Top Tracks" ] ]
                                    , li [] [ Html.a [onClick GetUserData, href "#getUserInfo" ] [ Html.text "User Info" ] ]
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
                          [ h1 [ class "title is-2" ] [ Html.text "KlangKapsel - Musik neu erleben" ]
                          , h2 [ class "subtitle is-4" ] [ Html.text "Ihr Soundtrack auf Knopfdruck" ]
                          , br [] []
                          , div [ class "column is-1 has-text-centered " ]
                              [ button [ onClick GetUserData,                                                    class "button is-medium is-success mt-2" ][ Html.text "Get User Data" ]
                              , Html.a [ onClick GetUserPlaylist, href "Https://xfronske.github.io/#getTopPlaylists", class "button is-medium is-success mt-2" ][ Html.text "my top Playlists" ]
                              , Html.a [ onClick GetTopTracks,    href "Https://xfronske.github.io/#getTopTracks",    class "button is-medium is-success mt-2" ][ Html.text "my Top Tracks" ]
                              , Html.a [                          href "Https://xfronske.github.io/#getUserInfo",     class "button is-medium is-success mt-2" ][ Html.text "my Profile" ]
                              , Html.a [                          href "Https://xfronske.github.io/#errorPage",       class "button is-medium is-success mt-2" ][ Html.text "Error Page (for testing)" ]
                              , Html.a [                          href "Https://xfronske.github.io/#musicPlayer",     class "button is-medium is-success mt-2" ][ Html.text "Music Player" ]

                              ]
                          ]
                    ]
                ]
            ]
        ,   footer [class "footer"]
        [ div [class "content has-text-centered"]
            [ p []
                [ strong [] [ Html.text "Klangkapsel"]
                , Html.text " by "
                , Html.a [href "https://eremiyarifat.de"] [ Html.text "Eremiya Rifat"]
                , Html.text " and Xaver Fronske. The source code is on "
                , Html.a [href "https://github.com/xfronske/MusicApp_uni/"] [ Html.text "GitHub"]
                , Html.text ". The website content is made with Elm "
                ]
            ]
        ]
    ]
    ]

playlistView : Model -> Html Msg
playlistView model = 
    div[class "columns"][
        div[class "column is-full"][
            h1[class "column is-size-1 is-full has-text-centered  "][ Html.text "Meine Playlists"],
             div [] (List.map viewPlaylistItem model.playlists)
        ]
    ]

viewPlaylistItem : Playlist -> Html Msg
viewPlaylistItem playlist =
    div [class "column is-full has-background-primary m-2 strong has-text-centered has-text-weight-bold"] [ Html.text playlist.name]      

viewTracks : Model -> Html Msg
viewTracks model =
    let
        histogram = model.topTracks |> popularityHistogram |> displayHistogram
        svgHistogram = histogramToSvg (popularityHistogram model.topTracks)
    in
    div [class "columns"]
        [ div [class "column is-full"]
            [ h1 [class "column is-size-1 is-full has-text-centered"] [ Html.text "Meine Top Tracks"]
            , div [] (List.map trackItemView model.topTracks)
            , histogram
            , div [] [  svgHistogram  ]
            ]
        ]

trackItemView : Track -> Html Msg
trackItemView track =
    div [class "column is-full  m-2 strong has-text-centered has-text-weight-bold"] [
            case List.drop 1 track.album.images |> List.head  of
            Just firstImage -> viewImage firstImage
            Nothing -> Html.text "",
              p [] [ Html.text (track.name ++ " - "  ) ]
            , p [] (List.map artistNames track.artists) 
            , p [] [ Html.text (String.fromInt track.popularity)]
        ]      

viewUserInfo : Model -> Html Msg
viewUserInfo model = 
    div[ class "columns"][
         div[class "column is-full"][
            h1 [class "column is-size-1 is-full has-text-centered"][ Html.text "Mein Profil"],
            div [class "column has-text-centered "][p[][strong[class "has-text-weight-bold"][ Html.text "Name : "], Html.text model.currentUser.display_name]],
            div [class "column has-text-centered "][p[][strong[class "has-text-weight-bold"][ Html.text "Email : "], Html.text model.currentUser.email]],
            div [class "column has-text-centered "][p[][strong[class "has-text-weight-bold"][ Html.text "Country : "], Html.text model.currentUser.country]]
             
            ]
        ]

viewImage : Image -> Html Msg
viewImage image =
    div []
        [ img [src image.url] [] ]

artistNames : Artist -> Html Msg
artistNames artists =
    span[][ Html.text artists.name]

-- Funktion zum Erzeugen der Anzeige eines Playlist-Namens

pageUserAccount : Model -> Html Msg 
pageUserAccount model = 
    div [][ tr [] [ Html.text ("email: " ++ model.currentUser.email)]
          , tr [] [ Html.text ("display name: " ++ model.currentUser.display_name)]
          , tr [] [ Html.text ("country:  " ++ model.currentUser.country)]
          , tr [] [ Html.text ("Spotify Id: " ++ model.currentUser.id)]
          ]

pageMusic : Model -> Html Msg 
pageMusic model = 
    div []
        [ th [][ button [ onClick IncVolume ][ Html.text "+" ] ]
        , th [][ if model.volume == 0 then svgVolumeNone 
                    else if model.volume > 0 && model.volume < 5 then svgVolumeLow 
                    else if model.volume >= 4 && model.volume <10 then svgVolumeMed 
                    else svgVolumeHigh 
               ] 
        , th [] [ button [ onClick DecVolume ][ Html.text "-" ] ]
        
        , Html.text (String.fromInt model.volume )

        , button [ class "button is-success is-outlined is-rounded is-small", onClick NextTrack ]
             [ Html.text "next" ]
        , button [ class "button is-success is-outlined is-rounded is-small", onClick PrevTrack ]
             [ Html.text "prev" ]
        , button [ class "button is-dark is-outlined is-rounded is-small", onClick TogglePlay ]
             [ if model.playbackState then Html.text "pause" else Html.text "play" ]
         ]


--##########.SVG.##########

svgVolumeNone = 
    svg [ SVG.width "40", SVG.height "20", SVG.viewBox "0 0 30 20", SVG.fill "black"]
        [ polyline [ SVG.fill "black", SVG.stroke "black", SVG.points "0,10 10,3 10,17 0,10" ] []
        , line [ SVG.x1 "15", SVG.y1 "5", SVG.x2 "25", SVG.y2 "15", SVG.stroke "black" ] []
        , line [ SVG.x1 "15", SVG.y1 "15", SVG.x2 "25", SVG.y2 "5", SVG.stroke "black" ] []
        ]

svgVolumeLow = 
    svg [ SVG.width "40", SVG.height "20", SVG.viewBox "0 0 30 20", SVG.stroke "black" ]
        [ polyline [ SVG.fill "black", SVG.points "0,10 10,3 10,17 0,10" ] []
        , polyline [ SVG.fill "none", SVG.points "15,6 17,9 18,10 17,11 15,14" ] []        
        ]

svgVolumeMed = 
    svg [ SVG.width "40", SVG.height "20", SVG.viewBox "0 0 30 20", SVG.stroke "black"]
        [ polyline [ SVG.fill "black", SVG.stroke "black", SVG.points "0,10 10,3 10,17 0,10" ] []
        , polyline [ SVG.fill "none", SVG.points "15,6 17,9 18,10 17,11 15,14" ] []
        , polyline [ SVG.fill "none", SVG.points "20,5 22,8 23,10 22,12 20,15" ] []
        ]

svgVolumeHigh = 
    svg [ SVG.width "40", SVG.height "20", SVG.viewBox "0 0 30 20", SVG.stroke "black"]
        [ polyline [ SVG.fill "black", SVG.stroke "black", SVG.points "0,10 10,3 10,17 0,10" ] []
        , polyline [ SVG.fill "none", SVG.points "15,6 17,9 18,10 17,11 15,14" ] []
        , polyline [ SVG.fill "none", SVG.points "20,5 22,8 23,10 22,12 20,15" ] []
        , polyline [ SVG.fill "none", SVG.points "25,4 27,7 28,10 27,13 25,16" ] [] ]

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
        { method = "GET"
        , headers = [Http.header "Authorization" model.token]
        , url = "https://api.spotify.com/v1/me/top/tracks"
        , body = Http.emptyBody
        , expect = Http.expectJson GotTopTracks topTracksResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

-- DECODER 

topTracksResponseDecoder : Decoder TopTracksResponse
topTracksResponseDecoder =
    Json.Decode.map TopTracksResponse
        (field "items" (Json.Decode.list trackDecoder))

trackDecoder : Decoder Track
trackDecoder =
    Json.Decode.map5 Track
        (field "id" string)
        (field "name" string)
        (field "artists" (Json.Decode.list artistDecoder))
        (field "album" albumDecoder)
        (field "popularity" int)
     
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
