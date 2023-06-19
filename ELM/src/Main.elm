module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, p, nav, a, span, i)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Time
import Task

--##########.MAIN.###########

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
        
        
type alias Model =
    { currentPage : Int
    , dropdownState : Bool 
    , zone : Time.Zone
    , time : Time.Posix
    }

--##########Page Index List#########
--##  0 -> Main                   ##
--##  1 -> Time (big)             ##
--##
--##################################


init : () -> (Model, Cmd Msg)
init _ =
  ( { currentPage = 0 -- 0 = Main Page
    , dropdownState = False
    , zone = Time.utc 
    , time = Time.millisToPosix 0
    }
  , Task.perform AdjustTimeZone Time.here
  )

--##########.Messages.and.Types.##########

type Msg
    = ToggleDropdown
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | TogglePage Int
    
--type Page 
    

    


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


    
navigation : Model -> Html Msg
navigation model = 
    div [class "dropdown_container"][ 
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
                                                          ][ text "MAIN"
                                                          ]
                                                      ]
                                                ]
                                          ]             
                              ]
                          ]
                    ]
              ]
        ]
          

view : Model -> Html Msg
view model =
  let
    hour   = String.fromInt (Time.toHour   model.zone model.time)
    minute = String.fromInt (Time.toMinute model.zone model.time)
    second = String.fromInt (Time.toSecond model.zone model.time)
  in
    
    div [][ navigation model 
          , div [][
                  text (hour ++ ":" ++ minute ++ ":" ++ second)
                  ]
          , case model.currentPage of 
                0 -> 
                    div[][text "Main Page"]
            
                1 -> 
                    div[][text "WOW THERE IS THE TIME"]
                
                _ ->
                    div [][text "page nothing"]
          ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick
  