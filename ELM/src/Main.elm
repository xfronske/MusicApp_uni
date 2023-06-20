port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, p, nav, a, span, i, blockquote)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
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
    
-- Ports
port sendMessage: String -> Cmd msg
-- if outgoing msg is 'requestToken' Token is gotten by JS side 
port messageReciever : (String -> msg) -> Sub msg

        
type alias Model =
    { currentPage : Int
    , dropdownState : Bool 
    , zone : Time.Zone
    , time : Time.Posix
    , currentQuote: Quote
    , httpState : HttpState
    
    -- Spotify API
    , lengthOfRandomString : Int

    -- flags
    , currentTime : Int

    -- ports
    , draft : String
    , message : String
    }


init : Int -> (Model, Cmd Msg)
init currentTime =
  ( { currentPage = 1 -- 0 = Main Page
    , dropdownState = False
    , zone = Time.utc 
    , time = Time.millisToPosix 0
    , currentQuote = { quote = ""
                     , source = ""
                     , author = ""
                     , year = 0
                     }
    , httpState = Http_Loading

    -- Spotify
    , lengthOfRandomString = 10
    
    -- Flags
    , currentTime = currentTime
    
    -- Ports
    , draft = ""
    , message = ""

    }
  , Task.perform AdjustTimeZone Time.here
  )

--##########.Messages.and.Types.##########

type Msg
    = ToggleDropdown
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | TogglePage Int
    
    -- Http Stuff
    | GetMore
    | GotQuote ( Result Http.Error Quote )
    
    -- Ports to JS
    | Send 
    | Recv String 
     
    
type HttpState
    = Http_Failure
    | Http_Loading
    | Http_Response Quote

type alias Quote = 
    { quote : String
    , source : String
    , author : String
    , year : Int
    }
    
    

    
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
            
         -- QUOTES   
        GetMore ->
            ( model 
            , getQuote
            )
            
        GotQuote result ->
        
            case result of 
                Ok quote -> 
                    ( { model | currentQuote = quote }
                    , Cmd.none
                    )
                    
                Err _ -> 
                    ( model
                    , Cmd.none
                    )
        
        -- Port to JS
        Send ->
            ( { model | draft = "" }
            , sendMessage model.draft
            )

        Recv newMessage ->
            ( { model | message = newMessage }
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
                                                          , onClick ( TogglePage 1 )
                                                          ][ text "ShowTime" ]        
                                                      ]
                                                , div [ class "dropdown-content" ][
                                                        a [ class "dropdown-item"
                                                          , onClick ( TogglePage 0 )
                                                          ][ text "MAIN" ]
                                                      ]
                                                , div [ class "dropdown-content" ][
                                                        a [ class "dropdown-item" 
                                                          , onClick ( TogglePage 2 )
                                                          ][ text "Http Page" ]
                                                      ]
                                                , div [ class "dropdown-content" ][
                                                        a [ class "dropdown-item" 
                                                          , onClick ( TogglePage 3 )
                                                          ][ text "Spotify" ]
                                                      ]
                                                ]
                                          ]             
                              ]
                          ]
                    ]
              ]
        ]
        
        
--##########.Quotes.##########

viewQuote : Model -> Html Msg
viewQuote model =

  case model.httpState of
    Http_Failure ->
      div []
        [ text "I could not load a random quote for some reason. "
        , button [ onClick GetMore ] [ text "Try Again!" ]
        ]

    Http_Loading ->
      div [][ text "Loading..."
          , button [ onClick GetMore ][ text "hhhhhh"]
          ]

    Http_Response quote ->
      div [][ 
            blockquote [] [ text quote.quote ] 
          ]
         
        
  
        
        
getQuote : Cmd Msg
getQuote = 
    Http.get 
        { url = "https://elm-lang.org/api/random-quotes"
        , expect = Http.expectJson GotQuote quoteDecoder
        }
        
        
quoteDecoder : Decoder Quote
quoteDecoder = 
    map4 Quote
        ( field "quote" string)
        ( field "source" string)
        ( field "author" string)
        ( field "year" int)

      
       
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
        
pageQuote : Model -> Html Msg
pageQuote model = 
    div [ class "container" ][
          viewQuote model
        ]
        
pageSpotify : Model -> Html Msg
pageSpotify model = 
    div [ class "container for spotify" ][
          text "Spotify API"
        , text model.message
        , text "hier sollte jtzt eigentlich was stehen"
        ]


--##########.VIEW.##########

--When adding a page you habe to ->
--    1. add a number in the index list 
--    2. add a page function
--    3. add dropdown thing

--##########Page Index List#########
--##  0 -> Main                   ##
--##  1 -> Time (big)             ##
--##  2 -> Http Stuff             ##
--##
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
                    div[][ pageQuote model ]

                3 ->
                    div[][ pageSpotify model ]
                
                _ ->
                    div [][text "page nothing"]
          ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick
  