port module Main exposing (main)

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
    
type alias Model =
    { currentPage : Int
    , dropdownState : Bool 
    
    -- Spotify
    , spotifydDropdownState : Bool
    , accountDropdownState : Bool
    , loginState : Bool
    , token : String
    , currentUser : UserData

    -- flags
    , currentTime : Int

    -- ports
    , message : String
    , accessToken: String
    , playlists : List Playlist
    }

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
    , playlists = []
    
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
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | TogglePage Int

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
                                                          ][ text "Main" ]        
                                                      ]

                                                      ]
                                                , div [ class "dropdown-content" ][
                                                        a [ class "dropdown-item" 
                                                          , onClick ( TogglePage 2 )
                                                          ][ text "Spotify" ]
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
                                                          [ text "My Accountdata" ]        
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

                   , if model.currentPage == 2 || model.currentPage == 3 then --Spotify page
                        div [ class "options for spotify" ][
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
                     else
                        div [][]



                    ] 
              ]
        ]
        
        

--##########.PAGES.##########

pageMain : Model -> Html Msg
pageMain model = 
    div [ class "container" ][
          text ("Main Page"++model.accessToken)
        ]
        
pageTime : Model -> Html Msg
pageTime model = 
    let
      hour   = String.fromInt (Time.toHour   model.zone model.time)
      minute = String.fromInt (Time.toMinute model.zone model.time)
      second = String.fromInt (Time.toSecond model.zone model.time)
    in
    div [ class "container" ][
          text "TIME" 

           
        , div [][
                  text (hour ++ ":" ++ minute ++ ":" ++ second)
                , text "Time from Flag"
                , text (String.fromInt model.currentTime)
                ]


        ]
        
    
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

    
    div [][ navigation model 
          , case model.currentPage of 
                0 -> 
                    div[][ pageMain model ]
            
                1 -> 
                    div[][ pageTime model ]
                
                2 ->
                    div[][ pageSpotify model ]

                3 ->
                    div[][ pageUserAccount model ]
                
                _ ->
                    div [][text "page nothing"]
          ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver RecFromJS



{-model =
  Time.every 1000 Tick-}