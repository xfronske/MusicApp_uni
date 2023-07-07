port module Main exposing (main)

import Browser
import Html exposing (..) --Html, button, div, Html.text, p, nav, a, span, i, blockquote, th, tr, input, ifIsEnter)
import Html.Attributes exposing (..)
import Html.Events exposing (..) --onClick, onInput)
import Json.Decode exposing (..)
import Json.Encode as Encode
import Task
import List exposing (..)
import Http exposing (..)
import Svg exposing (..)
import Svg.Attributes as SVG

--##########.MAIN.###########

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

    --SVG
    | ToggleAngle Int

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
    , currentUserArtist : Artist -- die ganze mybe kacke habe ich gemacht weil head nen meybe wert zurück gibt
    --ansonsten weiß ich nicht wie ich das darstellen kann

    --und die js ports funktionieren aber machen nichts weil ich das raus genommen habe das mache ich alles sauber wenn es funktioniert
    , searchArtistName : String
    , artists : List (Artist)

    -- flags
    , currentTime : Int

    -- SVG
    , angleState : Int

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
  ( { currentPage = 2 
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
    , currentUserArtist = { name = ""
                          , followers = { href = ""
                                        , total = 0
                                        } 
                          , id = ""
                          , href = ""
                          --, images : List Image  
                          } 

    --, currentUserArtist = Json.Decode.Field
    , searchArtistName = ""
    , playlists = []
    , artists = []
    
    -- Flags
    , currentTime = currentTime

    -- SVG
    , angleState = 0 -- rechts
    
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
                    ( { model | artists = data.items}
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
            ( { model | currentUserArtist = artist}
            , Cmd.none )

-- SVG 
        
        ToggleAngle state ->
            case model.angleState of 
                0 -> 
                    ( { model | angleState = state }
                    , Cmd.none )

                _->
                    ( model , Cmd.none ) 

       
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
                                                           span [][ svgAngleRight 
                                                                  , Html.text "navigation options" 
                                                                  ]
                                                         ]
                                                ]
                                          ]
                                
                                  True -> -- Dropdown offen
                                      div [ class "dropdown is-active" ][
                                            div [ class "dropdown-trigger", onClick ToggleNavigationDropdown][
                                                  button [ class "button is-success" ][
                                                           span [][ svgAngleDown
                                                                  , Html.text "navigation options"
                                                                  ]     
                                                         ]
                                                ]
                                          , div [ class "dropdown-menu"][
                                                  div [ class "dropdown-content" ][
                                                        Html.a [ class "dropdown-item" 
                                                               , onClick ( TogglePage 0 )
                                                               ][ Html.text "Spotify" ]        
                                                      ]

                                                , div [ class "dropdown-content" ][
                                                        Html.a [ class "dropdown-item" 
                                                               , onClick ( TogglePage 1 )
                                                               ][ Html.text "Spotify-data" ]
                                                      ]
                                                , div [ class "dropdown-content" ][
                                                        Html.a [ class "dropdown-item" 
                                                               , onClick ( TogglePage 2 )
                                                               ][ Html.text "SVG" ]
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
                                                           span [][ svgProfile
                                                                  , Html.text "Account"
                                                                ] 
                                                         , span [ class "icon is-small" ][ 
                                                                  i [ class " fas fa-angle-down"][]
                                                                ]    
                                                         ]
                                                ]
                                          , div [ class "dropdown-menu" ][
                                                  div [ class "dropdown-content" ][
                                                      Html.a [ class "dropdown-item", onClick LoadUserData ]
                                                             [ Html.text "My Account" ]        
                                                      ]
                                                
                                                , div [ class "dropdown-content" ][
                                                      Html.a [ class "dropdown-item", onClick LogoutFromSpotify ][
                                                             Html.text "Logout"
                                                            ]
                                                      ]
                                                ]
                                          ]
                                    False -> --Dropdown zu
                                      div [ class "dropdown" ][
                                            div [ class "dropdown-trigger", onClick ToggleAccountDropdown ][
                                                  button [ class "button" ][
                                                           span [][ svgProfile
                                                                  , Html.text "Account" 
                                                                  ]
                                                         , span [ class "icon is-small" ][
                                                                  i [ class "fas fa-angle-down" ][]
                                                                ]
                                                         ]
                                                ]
                                          ]             
                              ]

                            else 
                                p [] [Html.text "you are not logged in"]
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
                                                                       span [][ Html.text "Spotify Options" ]
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
                                                                              Html.text "spotify options"
                                                                            ] 
                                                                     , span [ class "icon is-small" ][ 
                                                                              i [ class " fas fa-angle-down"][]
                                                                            ]    
                                                                     ]
                                                            ]
                                                      , div [ class "dropdown-menu"][
                                                              div [ class "dropdown-content" ][
                                                                    Html.a [ class "dropdown-item" 
                                                                      , onClick ( TogglePage 0 )
                                                                      ][ Html.text "Search artist" ]        
                                                                  ]
                                                            , div [ class "dropdown-content" ][
                                                                    Html.a [ class "dropdown-item"
                                                                      , onClick ( TogglePage 1 )
                                                                      ][ Html.text "Search song" ]
                                                                  ]
                                                            , div [ class "dropdown-content" ][
                                                                    Html.a [ class "dropdown-item" 
                                                                      , onClick ( TogglePage 2 )
                                                                      ][ Html.text "Search album" ]
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
    div [ class "container for spotify" ][
          input [ type_ "Html.text"
                , placeholder "Artist Name"
                , onInput ChangeArtist
                , on"keydown" (ifIsEnter GetArtist)
                , Html.Attributes.value model.searchArtistName ]
                []
        , button [ onClick GetArtist ] [ Html.text "search" ] 

        , Html.text ("your artist: "  )
        
        ]


pageUserAccount : Model -> Html Msg 
pageUserAccount model = 
    div [][ tr [] [ Html.text ("email: " ++ model.currentUser.email)]
          , tr [] [ Html.text ("display name: " ++ model.currentUser.display_name)]
          , tr [] [ Html.text ("country:  " ++ model.currentUser.country)]
          , tr [] [ Html.text ("Spotify Id: " ++ model.currentUser.id)]
        ]

pageSvg : Model-> Html Msg 
pageSvg model = 
    div [][ svgAngleDown
          , svgAngleRight
          , svgProfile
          ]


--##########.SVG.##########


{-optionsIcon = 
    svg [ SVG.height "130", SVG.width "130", SVG.viewBox "0 0 60 60"]
        [ line [ SVG.x "10", SVG.y "20" ] ]
-}
       -- <path fill="none" stroke="#fff" stroke-width="5" stroke-linejoin="bevel" 
       -- d="M5.0916789,20.818994C5.0916789,20.818994,58.908321,20.818994,58.908321,20.818994">
    
svgAngleRight = 
    svg [ SVG.width "20", SVG.height "20", SVG.viewBox "0 0 25 20", SVG.fill "fffff"]
        [ Svg.line [SVG.x1 "05", SVG.y1 "10", SVG.x2 "15", SVG.y2 "15", SVG.stroke "black"] [] 
        , Svg.line [SVG.x1 "05", SVG.y1 "20", SVG.x2 "15", SVG.y2 "15", SVG.stroke "black"] []
        ]

svgAngleDown = 
    svg [ SVG.width "20", SVG.height "20", SVG.viewBox "0 0 25 20", SVG.fill "fffff"]
        [ Svg.line [SVG.x1 "05", SVG.y1 "10", SVG.x2 "10", SVG.y2 "20", SVG.stroke "black"] [] 
        , Svg.line [SVG.x1 "15", SVG.y1 "10", SVG.x2 "10", SVG.y2 "20", SVG.stroke "black"] []
        ]

svgProfile = 
    svg [SVG.width "20", SVG.height "20", SVG.viewBox "0 0 20 20" ]
        [ circle [SVG.cx "10", SVG.cy "10", SVG.r "09", SVG.fill "none", SVG.stroke "black"] [] 
        , circle [SVG.cx "10", SVG.cy "7", SVG.r "4", SVG.fill "none", SVG.stroke "black"] []
        , line [ SVG.x1 "4", SVG.y1 "14", SVG.x2 "16", SVG.y2 "14", SVG.stroke "black"] []
        ]

{-



        -}


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

                2 ->
                    div[][ pageSvg model ]
                
                _ ->
                    div [][Html.text "page nothing"]
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