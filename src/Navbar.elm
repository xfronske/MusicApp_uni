module Navbar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)

view : Html msg
view =
    nav []
        [ a [ href "#" ] [ text "Navbar" ]
        ]
