port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, p, nav, a, span, i, blockquote, th, tr, img)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Json.Encode as Encode
import Time
import Task
import Http exposing (..)

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
    , token : String
    , currentUser : UserData
    , topArtist : UserTopArtist 

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
    , token = ""
    , currentUser = { country = ""
                    , display_name = ""
                    , email = ""
                    , id = "" }
    , topArtist = { href = ""
                  , name = ""
                  , image =  { url = ""
                             , height = 0 
                             , width = 0 }
                  }
    
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
    | RecFromJS String 

    -- Spotify
    | ToggleLoginState Bool
    | LoadUserData
    | GotUserData (Result Http.Error UserData) 
    | LoadUserTopArtist
    | GotUserTopArtist (Result Http.Error UserTopArtist)
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

        RecFromJS token ->
            ( { model | accessToken = token
                      , token = "Bearer " ++ token
                      , loginState = True }
            , Cmd.none )

        ToggleLoginState state ->
            ( { model | loginState = state }
            , Cmd.none )


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

        LoadUserTopArtist -> 
            ( { model | currentPage = 2 }
            , ( getUserTopArtist model) )

        GotUserTopArtist artistData ->
            case artistData of 
                Ok data -> 
                    ( { model | topArtist = UserTopArtist data.name data.href data.image}
                    , Cmd.none )

                Err _ -> 
                    ( model, Cmd.none)

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
                                button [ class "button", onClick LoginToSpotify ][ text "Login to Spotify" ]
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
          text "This is the main Page"
        ]
    
pageSpotify : Model -> Html Msg
pageSpotify model = 
    div [ class "container for spotify" ][
          text ( "Token: " ++ model.accessToken)
        , button [ onClick RecieveToken ][text "get"]
        , button [ onClick LoadUserTopArtist ][text "top artist"]
        , tr [] [ text ("name: " ++ model.topArtist.name)]
        , tr [] [ text ("display name: " ++ model.topArtist.href)]
        , img [ src  model.topArtist.image.url][]
        ]

pageUserAccount : Model -> Html Msg 
pageUserAccount model = 
    div [][ tr [] [ text ("email: " ++ model.currentUser.email)]
          , tr [] [ text ("display name: " ++ model.currentUser.display_name)]
          , tr [] [ text ("country:  " ++ model.currentUser.country)]
          , tr [] [ text ("Spotify Id: " ++ model.currentUser.id)]
        ]

pageUserTopArtist : Model -> Html Msg 
pageUserTopArtist model = 
    div [][ tr [] [ text ("name: " ++ model.topArtist.name)]
          , tr [] [ text ("display name: " ++ model.topArtist.href)]
          , img [ src  model.topArtist.image.url][]
          ]

--##########.Spotify.stuff.##########

-- User Data

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

-- User top Artist

type alias UserTopArtist =
    { href : String
    , name : String
    , image : ArtistImage }

type alias ArtistImage = 
    { url : String
    , height : Int 
    , width : Int }

getUserTopArtist : Model -> Cmd Msg 
getUserTopArtist model = 
    Http.request 
      { method = "GET"
      , headers = [Http.header "Authorization" model.token]
      , url = "hhttps://api.spotify.com/v1/me/top/artists?limit=1&offset=0"
      , body = Http.emptyBody
      , expect = Http.expectJson GotUserTopArtist decodeUserTopArtist
      , timeout = Nothing
      , tracker = Nothing
      }

decodeUserTopArtist : Decoder UserTopArtist
decodeUserTopArtist = 
    Json.Decode.map3 UserTopArtist
        (field "name" string)
        --(field "genres" ( string))
        (field "href" string)
        (field "images" decodeTopArtistImage)

decodeTopArtistImage : Decoder ArtistImage
decodeTopArtistImage = 
    Json.Decode.map3 ArtistImage 
        (field "url" string)
        (field "height" int)
        (field "width" int)


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
                    div[][ pageMain model ]
                        
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