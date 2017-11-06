module Playground exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Transform exposing (..)


main =
    div [ class "board" ] [ toHtml board ]


boardSize =
    800


board =
    collage boardSize boardSize [ makePath ]


makePath =
    path [ ( 0, 0 ), ( 0, 50 ), ( 0, 80 ), ( 5, 90 ) ] |> traced (lineStyle blue)


lineStyle color =
    { defaultLine
        | width = 10
        , color = color
        , cap = Round
        , join = Smooth
    }



-- make the pth point


mkPoint : Float -> Float -> ( Float, Float )
mkPoint t p =
    ( 10 * p / 3
    , 10 * sin t * sin (p / 3)
    )



-- make a list of points for each time in the animation


points : Float -> List ( Float, Float )
points t =
    List.map (mkPoint t) <|
        List.map toFloat <|
            List.range 0 50
