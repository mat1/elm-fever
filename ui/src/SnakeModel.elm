module SnakeModel exposing (..)

import Keyboard.Extra exposing (Key(..))
import Time exposing (Time)
import Color exposing (Color)


type alias Snake =
    { points : List ( Float, Float )
    , angle : Float
    , state : SnakeState
    , name : String
    , color : Color
    , left : Key
    , right : Key
    , rank : Int
    , snakePlayer : SnakePlayer
    , direction : Direction
    }


type SnakePlayer
    = Self
    | Other


type SnakeState
    = Running
    | GameOver


type Direction
    = Left
    | Straight
    | Right


toDirection str =
    if str == "Left" then
        Left
    else if str == "Right" then
        Right
    else
        Straight
