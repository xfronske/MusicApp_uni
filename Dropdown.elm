module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, p, nav, a, span, i)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
        
        
type alias Model =
    { dropdownState : Bool }


initialModel : Model
initialModel =
    { dropdownState = False }


type Msg
    = ToggleDropdown

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDropdown ->
            { model | dropdownState = not model.dropdownState}


    
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
    div [][ navigation model ]


