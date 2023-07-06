port module Main exposing (main, subscriptions)

import Browser
import Html exposing (Html, button, div, text, p, nav, a, span, i, blockquote, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Json.Encode as Encode
import Task
import Http exposing (..)

--##########.MAIN.###########
-- TODO : FrontEnd für Main Page 
-- kümmert sich Xaaver um getPlaylists ?
    -- ja tut er

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
    
--##########.Ports
port sendMessage: String -> Cmd msg

port messageReceiver : (String -> msg) -> Sub msg
port loginStatePort : (String -> msg) -> Sub msg
type alias Model =
    { currentPage : Int
    , dropdownState : Bool 
    
    -- Spotify
    , spotifydDropdownState : Bool
    , accountDropdownState : Bool
    , loginState : Bool
    , token : String
    , currentUser : UserData,
      topTracks : List Track

    -- flags
    , currentTime : Int

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
    artists  : List Artist

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

init : Int -> (Model, Cmd Msg)
init currentTime =
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
    , playlists = [],
     topTracks = []
    
    -- Flags
    , currentTime = currentTime
    
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

       
--##########.Navbar.uuuuh.##########
    
navigation : Model -> Html Msg
navigation model = 
    div [][text "asd"]
        

--##########.PAGES.##########

pageMain : Model -> Html Msg
pageMain model = 
    div [ class "container" ][
          text ("Main Page"++model.accessToken),
          p [][text ("User Data : " ++ model.currentUser.display_name)],
          button [onClick LoadUserData][text "getuserData"],
          button [onClick LoadUserPlaylist][text "getuserPlaylist"],
          p [][text ("Playlist: ")],
          div [] (List.map playlistNameView model.playlists),
          button [ onClick LoadTopTracks ] [ text "Top-Tracks laden" ],
            div []  (List.map trackView model.topTracks)
               
        ]
trackView : Track -> Html Msg
trackView track =
    div []
        [
            p [] [ text (track.name ++ " - "  ) ]
            , p[] (List.map artistNames track.artists) 
        ]
artistNames : Artist -> Html Msg
artistNames artists =
    span[][text artists.name]
-- Funktion zum Erzeugen der Anzeige eines Playlist-Namens
playlistNameView : Playlist -> Html Msg
playlistNameView playlist =
    div [] [text playlist.name]      
  
    
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
    Json.Decode.map3 Track
        (field "id" string)
        (field "name" string)
        (field "artists" (Json.Decode.list artistDecoder))
     
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

decodeUserPlaylists : Decoder UserData
decodeUserPlaylists = 
    Json.Decode.map4 UserData
        (field "country" string)
        (field "display_name" string)
        (field "email" string)
        (field "id" string)


decodeUserData : Decoder UserData
decodeUserData = 
    Json.Decode.map4 UserData
        (field "country" string)
        (field "display_name" string)
        (field "email" string)
        (field "id" string)


--##########.VIEW.##########

--When adding a page you habe to ->
--    1. add a number in the index list 
--    2. add a page function
--    3. add dropdown-item

--##########Page Index List#########
--##  0 -> Main                   ##
--##  1 -> Time (big)             ##
--##  2 -> Spotify                ##
--##  3 -> User Account           ##
--##  4 -> 
--##################################

view : Model -> Html Msg
view model =

    
    div [][ 
                    div[][ pageMain model ]

          ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver RecFromJS



{-model =
  Time.every 1000 Tick-}