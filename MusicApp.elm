module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, p, nav, a, span, i)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Time
import Task


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
        
        
type alias Model =
    { dropdownState : Bool 
    , zone : Time.Zone
    , time : Time.Posix
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( { dropdownState = False
    , zone = Time.utc 
    , time = Time.millisToPosix 0
    }
  , Task.perform AdjustTimeZone Time.here
  )

type Msg
    = ToggleDropdown
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone

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


    
navigation : Model -> Html Msg
navigation model = 
    div [class "dropdown_container"][ 
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
                                      (text "print followers")
                                    ]
                              , div [ class "dropdown-content" ][
                                      (text "zwei2")
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
          , text (hour ++ ":" ++ minute ++ ":" ++ second)
          ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick
  