module SnakeModel exposing (..)

import Keyboard.Extra exposing (Key(..))
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
    , player : Player
    }


type SnakeState
    = Running
    | GameOver


type Player
    = Human
    | Bot
