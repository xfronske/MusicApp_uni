port module Main exposing (main)

import Browser
import Html exposing (..) --Html, button, div, text, p, nav, a, span, i, blockquote, th, tr, input, ifIsEnter)
import Html.Attributes exposing (..)
import Html.Events exposing (..) --onClick, onInput)
import Json.Decode exposing (..)
import Json.Encode as Encode
import Task
import List exposing (..)
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
    
--##########.Ports.##########
-- out
port sendMessage: String -> Cmd msg
port sendArtist: String -> Cmd msg

-- in
port messageReceiver : (String -> msg) -> Sub msg
port recieveArtist: (Json.Decode.Value -> msg) -> Sub msg 
    

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
    | ChangeArtist String
    | SendArtistToJS
    | RecArtist Artist

    -- Spotify
    | ToggleLoginState Bool
    | LoadUserData
    | GotUserData (Result Http.Error UserData) 
    | GetArtist
    | GotArtist (Result Http.Error ArtistResponse)
    | ToggleUserPage

    --JIM
    | GotPlaylists (Result Http.Error PlaylistResponse)
    | InitiatePlaylistFetch

type alias Model =
    { currentPage : Int
    , dropdownState : Bool 
    
    -- Spotify
    , spotifydDropdownState : Bool
    , accountDropdownState : Bool
    , loginState : Bool
    , token : String
    , currentUser : UserData
    , currentUserArtist : Maybe Artist -- die ganze mybe kacke habe ich gemacht weil head nen meybe wert zurück gibt
    --ansonsten weiß ich nicht wie ich das darstellen kann
    , searchArtistName : String
    , artists : List (Artist)

    -- flags
    , currentTime : Int

    -- ports
    , message : String
    , accessToken: String
    , playlists : List Playlist
    }

type alias Playlist =
    { id : String
    , name : String
    , href : String
    }

type alias PlaylistResponse =
    { items : List Playlist }

type alias ArtistResponse = 
    { items : List Artist}

type alias Artist = 
    { name : String 
    , followers : Followers
    , id : String
    , href : String
    --, genres : List String    
    --, images : List Image  
    } 

type alias Followers =
    { href : String
    , total : Int }

type alias Image = 
    { url : String
    , height : Int 
    , width : Int }


--##########.Init.##########

init : Int -> (Model, Cmd Msg)
init currentTime =
  ( { currentPage = 0 
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
    , currentUserArtist = Nothing{-{ name = Nothing
                          , followers = { href = Nothing
                                        , total = Nothing
                                        } 
                          , id = Nothing
                          , href = Nothing  
                          --, images : List Image  
                          } -}

    --, currentUserArtist = Json.Decode.Field
    , searchArtistName = ""
    , playlists = []
    , artists = []
    
    -- Flags
    , currentTime = currentTime
    
    -- Ports
    , message = ""
    , accessToken = ""

    }
  , Cmd.none
  )
     
    
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

        ToggleUserPage ->
            ( { model | currentPage = 0 }
            , Cmd.none )

        LoadUserData ->
            ( { model | currentPage = 1 } 
            , ( getUserData model) )

        GotUserData userData ->
            case userData of 
                Ok data -> 
                    ( { model | currentUser = UserData data.country data.display_name data.email data.id} 
                    , Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetArtist ->
            ( model
            , (getArtist model) )

        GotArtist userArtist ->
            case userArtist of 
                Ok data ->
                    ( { model | currentUserArtist = head data.items}
                    , Cmd.none )
                    {-( {model | currentUserArtist = Artist data.name data.followers data.id data.href}
                    , Cmd.none ) -}

                Err _ ->
                    (model , Cmd.none )

        ChangeArtist artistName ->
            ( { model | searchArtistName = artistName }
            , Cmd.none )

        SendArtistToJS -> 
            ( model --| searchArtistName = ""
            , sendArtist model.searchArtistName )

        RecArtist artist ->
            ( { model | currentUserArtist = Nothing}
            , Cmd.none )

        

       
--##########.Navbar.uuuuh.##########
    
navigation : Model -> Html Msg
navigation model = 
    div [class ""][ 
        div [ class "container" ][
                nav [ class "level" ][
                      div [ class "level-left" ][
                            p [ class "level-item"][
           
                                case model.dropdownState of 
                                  False -> --Dropdown zu
                                      div [ class "dropdown" ][
                                            div [ class "dropdown-trigger", onClick ToggleNavigationDropdown][
                                                  button [ class "button is-success" ][
                                                           span [][ text "navigation options" ]
                                                         , span [ class "icon is-small" ][
                                                                  i [ class "fas fa-angle-down" ][]
                                                                ]
                                                         ]
                                                ]
                                          ]
                                
                                  True -> -- Dropdown offen
                                      div [ class "dropdown is-active" ][
                                            div [ class "dropdown-trigger", onClick ToggleNavigationDropdown][
                                                  button [ class "button is-success" ][
                                                           span [][
                                                                  text "navigation options"
                                                                ] 
                                                         , span [ class "icon is-small" ][ 
                                                                  i [ class " fas fa-angle-down"][]
                                                                ]    
                                                         ]
                                                ]
                                          , div [ class "dropdown-menu"][
                                                  div [ class "dropdown-content" ][
                                                        a [ class "dropdown-item" 
                                                          , onClick ( TogglePage 0 )
                                                          ][ text "Spotify" ]        
                                                      ]

                                                , div [ class "dropdown-content" ][
                                                        a [ class "dropdown-item" 
                                                          , onClick ( TogglePage 1 )
                                                          ][ text "Spotify----" ]
                                                      ]
                                                ]
                                          ]             
                              ]
                          ]
                    -- LEVEL RIGHT ----------------------Account Dropdown
                    , div [ class "level-right"][
                            if model.loginState then
                                p [ class "level-item"][  
                                  case model.accountDropdownState of 
                                    True ->                            
                                      div [ class "dropdown is-active" ][
                                            div [ class "dropdown-trigger", onClick ToggleAccountDropdown ][
                                                  button [ class "button" ][
                                                           span [][
                                                                  text "Account"
                                                                ] 
                                                         , span [ class "icon is-small" ][ 
                                                                  i [ class " fas fa-angle-down"][]
                                                                ]    
                                                         ]
                                                ]
                                          , div [ class "dropdown-menu" ][
                                                  div [ class "dropdown-content" ][
                                                        a [ class "dropdown-item", onClick LoadUserData ]
                                                          [ text "My Account" ]        
                                                      ]
                                                
                                                , div [ class "dropdown-content" ][
                                                        a [ class "dropdown-item", onClick LogoutFromSpotify ][
                                                            text "Logout"
                                                          ]
                                                      ]
                                                ]
                                          ]
                                    False -> --Dropdown zu
                                      div [ class "dropdown" ][
                                            div [ class "dropdown-trigger", onClick ToggleAccountDropdown ][
                                                  button [ class "button" ][
                                                           span [][ text "Account" ]
                                                         , span [ class "icon is-small" ][
                                                                  i [ class "fas fa-angle-down" ][]
                                                                ]
                                                         ]
                                                ]
                                          ]             
                              ]

                            else 
                                p [] [text "ho"]
                          ]

                   ,  div [ class "options for spotify" ][
                          div [ class "container" ][
                            nav [ class "level" ][
                                  div [ class "level-left" ][
                                        p [ class "level-item"][
                       
                                            case model.spotifydDropdownState of 
                                              False -> --Dropdown zu
                                                  div [ class "dropdown" ][
                                                        div [ class "dropdown-trigger", onClick ToggleSpotifyDropdown][
                                                              button [ class "button" ][
                                                                       span [][ text "Spotify Options" ]
                                                                     , span [ class "icon is-small" ][
                                                                              i [ class "fas fa-angle-down" ][]
                                                                            ]
                                                                     ]
                                                            ]
                                                      ]
                                            
                                              True -> -- Dropdown offen
                                                  div [ class "dropdown is-active" ][
                                                        div [ class "dropdown-trigger", onClick ToggleSpotifyDropdown][
                                                              button [ class "button" ][
                                                                       span [][
                                                                              text "spotify options"
                                                                            ] 
                                                                     , span [ class "icon is-small" ][ 
                                                                              i [ class " fas fa-angle-down"][]
                                                                            ]    
                                                                     ]
                                                            ]
                                                      , div [ class "dropdown-menu"][
                                                              div [ class "dropdown-content" ][
                                                                    a [ class "dropdown-item" 
                                                                      , onClick ( TogglePage 0 )
                                                                      ][ text "Search artist" ]        
                                                                  ]
                                                            , div [ class "dropdown-content" ][
                                                                    a [ class "dropdown-item"
                                                                      , onClick ( TogglePage 1 )
                                                                      ][ text "Search song" ]
                                                                  ]
                                                            , div [ class "dropdown-content" ][
                                                                    a [ class "dropdown-item" 
                                                                      , onClick ( TogglePage 2 )
                                                                      ][ text "Search album" ]
                                                                  ]
                                                            ]
                                                      ]             
                                         ]
        
                                ]
                    ]
              ]
            ]      
                    ] 
              ]
        ]
        
        

--##########.PAGES.##########
    
pageSpotify : Model -> Html Msg
pageSpotify model = 
    let 
        te = Maybe.fromJust model.currentUserArtist
    in 

    div [ class "container for spotify" ][
          button [ onClick GetArtist ][ text "Artist"]

        , input [ type_ "text"
                , placeholder "Artist Name"
                , onInput ChangeArtist
                , on"keydown" (ifIsEnter GetArtist)
                , Html.Attributes.value model.searchArtistName ]
                []
        , button [ onClick GetArtist ] [ text "search" ] 

        , text ("your artist: " ++ te )
        
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
    
type alias UserData = 
    { country : String
    , display_name : String
    , email : String
    , id : String }

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

decodeUserData : Decoder UserData
decodeUserData = 
    Json.Decode.map4 UserData
        (field "country" string)
        (field "display_name" string)
        (field "email" string)
        (field "id" string)



-- Artists top

getArtist : Model -> Cmd Msg 
getArtist model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization"  model.token ]
        , url = "https://api.spotify.com/v1/search?q=" ++ model.searchArtistName ++ "&type=artist"
        , body = Http.emptyBody
        , expect = Http.expectJson GotArtist decodeArtistResponse
        , timeout = Nothing
        , tracker = Nothing
        }

decodeArtistResponse : Decoder ArtistResponse
decodeArtistResponse = 
    Json.Decode.map ArtistResponse
        (field "items" (Json.Decode.list decodeArtist))

decodeArtist : Decoder Artist
decodeArtist = 
    Json.Decode.map4 Artist
        (field "name" string)
        (field "followers" decodeFollowers)
        (field "id" string)
        (field "href" string)
        --(field "genres" decodeGenres (Html.Attributes.list string) "[]")
        --(field "images" decodeImage  (Html.Attributes.list string) "[url,height,width]") 

decodeFollowers : Decoder Followers
decodeFollowers = 
    Json.Decode.map2 Followers 
        (field "href" string)
        (field "total" int ) 


{-
decodeImage : Decoder Image 
decodeImage = 
    Json.Decode.map3 Image 
        (field "url" string)
        (field "width" int)
        (field "height" int)

decodeGenres : Decoder Genres 
decodeGenres = 
-}

-- Playlists

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




--##########.VIEW.##########

--When adding a page you habe to ->
--    1. add a number in the index list 
--    2. add a page function
--    3. add dropdown-item

--##########Page Index List#########
--##  0 -> Main                   ##
--##  2 -> Spotify                ##
--##  3 -> User Account           ##
--##  4 -> 
--##################################

view : Model -> Html Msg
view model =

    
    div [][ navigation model 
          , case model.currentPage of                 
                0 ->
                    div[][ pageSpotify model ]

                1 ->
                    div[][ pageUserAccount model ]
                
                _ ->
                    div [][text "page nothing"]
          ]



-- HELP FUNCITIONS

ifIsEnter: msg -> Decoder msg 
ifIsEnter msg = 
    Json.Decode.field "key" string
        |> Json.Decode.andThen (\key -> if key == "Enter"then Json.Decode.succeed msg else Json.Decode.fail "some othern key")

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ messageReceiver RecFromJS, Sub.none ]--recieveArtist ( RecArtist << decodeValue(Artist))]