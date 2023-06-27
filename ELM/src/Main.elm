port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, p, nav, a, span, i, blockquote)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Time
import Task
import Http

--##########.MAIN.###########

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
    , zone : Time.Zone
    , time : Time.Posix
    
    -- Spotify
    , spotifydDropdownState : Bool

    -- flags
    , currentTime : Int

    -- ports
    , message : String
    , accessToken: String
    }


init : Int -> (Model, Cmd Msg)
init currentTime =
  ( { currentPage = 2 -- 0 = Main Page
    , dropdownState = False
    , zone = Time.utc 
    , time = Time.millisToPosix 0

    -- Spotify
    , spotifydDropdownState = False
    
    -- Flags
    , currentTime = currentTime
    
    -- Ports
    , message = ""
    , accessToken = ""

    }
  , Task.perform AdjustTimeZone Time.here
  )

--##########.Messages.and.Types.##########

type Msg
    = ToggleDropdown
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | TogglePage Int
    
    -- Ports to JS
    | LoginToSpotify
    | LogoutFromSpotify
    | RefreshToken
    | RecieveToken
    | Recv String 

    -- Spotify
    | ToggleSpotifyDropdown
     
    
--##########.Update.##########

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggleDropdown ->
            ( { model | dropdownState = not model.dropdownState}
            , Cmd.none
            )
            
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )
            
        TogglePage newPage->
            ( { model | currentPage = newPage }
            , Cmd.none
            )
            
-- Spotify

        ToggleSpotifyDropdown ->
            ( { model | spotifydDropdownState = not model.spotifydDropdownState }
            , Cmd.none )

        LoginToSpotify ->
            ( model
            , sendMessage "login")

        LogoutFromSpotify ->
            ( model
            , sendMessage "logout")

        RefreshToken ->
            ( model 
            , sendMessage "refreshToken")

        RecieveToken -> 
            ( model 
            , sendMessage "recieveToken")

        Recv token ->
            ( { model | accessToken = token }
            , Cmd.none 
            )

       
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
                                            div [ class "dropdown-trigger", onClick ToggleDropdown][
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
                                            div [ class "dropdown-trigger", onClick ToggleDropdown][
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
                                                , div [ class "dropdown-content" ][
                                                        a [ class "dropdown-item"
                                                          , onClick ( TogglePage 1 )
                                                          ][ text "Time" ]
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
                    ]
              ]
        ]
        
        

--##########.PAGES.##########

pageMain : Model -> Html Msg
pageMain model = 
    div [ class "container" ][
          text "This is the main Page"
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

        , button [ onClick LoginToSpotify ][text "Login to Spotify"]
        , button [ onClick LogoutFromSpotify ][text "Logout"]
        , button [ onClick RecieveToken ][text "get"]
        , text ("Access Token: " ++ model.accessToken)
        ]

--##########.Spotify.stuff.##########
  {-function getUserData() {
    fetch('https://api.spotify.com/v1/me', {
      headers: {
        Authorization: 'Bearer ' + access_token,
      },
    })
      .then(async (response) => {
        if (response.ok) {
          return response.json();
        } else {
          throw await response.json();
        }
      })
      .then((data) => {
        console.log(data);
      })
      .catch((error) => {
        console.error(error);
      });
  }
else if (access_token && refresh_token && expires_at) {

    // if already authorized then reload tokens from localStorage
    getUserData();
  }
  -}


--##########.VIEW.##########

--When adding a page you habe to ->
--    1. add a number in the index list 
--    2. add a page function
--    3. add dropdown-item

--##########Page Index List#########
--##  0 -> Main                   ##
--##  1 -> Time (big)             ##
--##  2 -> Spotify                ##
--##  3 -> 
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
                
                _ ->
                    div [][text "page nothing"]
          ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver Recv


{-model =
  Time.every 1000 Tick-}