port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, p, nav, a, span, i, blockquote)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
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

port loginStatePort : (String -> msg) -> Sub msg

        
type alias Model =
    { currentPage : Int
    , dropdownState : Bool 
    , zone : Time.Zone
    , time : Time.Posix
    
    -- Spotify
    , spotifydDropdownState : Bool
    , accountDropdownState : Bool
    , loginState : Bool

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
    , accountDropdownState = False
    , loginState = False
    
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
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | TogglePage Int

    -- Dropdown
    | ToggleNavigationDropdown
    | ToggleSpotifyDropdown
    | ToggleAccountDropdown
    
    -- Ports to JS
    | LoginToSpotify
    | LogoutFromSpotify
    | RefreshToken
    | RecieveToken
    | Recv String 

    -- Spotify
    | ToggleLoginState Bool
    | LoadUserData
    | GotUserData (Result Http.Error UserData)
    | ToggleUserPage
     
    
--##########.Update.##########

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggleNavigationDropdown ->
            ( { model | dropdownState = not model.dropdownState}
            , Cmd.none )
            
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
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

        LoginToSpotify ->
            ( model
            , sendMessage "login" )

        LogoutFromSpotify ->
            ( { model | accessToken = "" }
            , sendMessage "logout" )

        RefreshToken ->
            ( model 
            , sendMessage "refreshToken" )

        RecieveToken -> 
            ( model 
            , sendMessage "recieveToken" )

        Recv token ->
            ( { model | accessToken = token
                      , loginState = True }
            , Cmd.none )

        ToggleLoginState state ->
            ( { model | loginState = state }
            , Cmd.none )

        LoadUserData ->
            ( { model | currentPage = 3 } 
            , ( getUserData model) )

        GotUserData userData ->
            ( model 
            , Cmd.none )

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
                    -- LEVEL RIGHT ----------------------
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
                                button [ class "button", onClick LoginToSpotify ][ text "Login to Spotify" ]
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

        , button [ onClick RecieveToken ][text "get"]
        ]

pageUserAccount : Model -> Html Msg 
pageUserAccount model = 
    div [][ text "user information"]

--##########.Spotify.stuff.##########

getUserData : Model -> Cmd Msg  
getUserData model = 
    let 
        header = Http.header("Authorization: Bearer " ++ model.accessToken)
    in
  Http.request
    { method = "GET"
    , headers = [  ]
    , url = "https://api.spotify.com/v1/me"
    , body = Http.emptyBody
    , expect = Http.expectJson GotUserData decodeUserData
    , timeout = Nothing
    , tracker = Nothing
    }

type alias UserData = 
    { country : String
    , display_name : String
    , email : String
    , id : String}

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
  messageReceiver Recv



{-model =
  Time.every 1000 Tick-}