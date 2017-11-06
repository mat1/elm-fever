module Snake exposing (..)

import Keyboard.Extra exposing (Key(..))
import Constants exposing (..)


type alias Snake =
    { points : List ( Float, Float )
    , angle : Float
    , state : SnakeState
    , name : String
    }


type SnakeState
    = Running
    | GameOver


updateSnake snake pressedKeys deltaTime =
    case snake.state of
        GameOver ->
            snake

        Running ->
            updateSnakePoision snake pressedKeys deltaTime


updateSnakePoision snake pressedKeys deltaTime =
    let
        ( lastX, lastY ) =
            List.head snake.points |> Maybe.withDefault ( 0, 0 )

        angle =
            snake.angle + (angleSpeed * getDirection pressedKeys * deltaTime)

        x =
            lastX + (speed * cos (angle * pi / 180) * deltaTime)

        y =
            lastY + (speed * sin (angle * pi / 180) * deltaTime)
    in
        { snake
            | points = ( x, y ) :: snake.points
            , angle = angle
            , state = updateSnakeState ( x, y ) (List.drop 10 snake.points)
        }


updateSnakeState ( x, y ) points =
    if collision ( x, y ) points then
        GameOver
    else
        Running


getDirection pressedKeys =
    if leftPressed pressedKeys then
        1
    else if rightPressed pressedKeys then
        -1
    else
        0


collision ( x, y ) points =
    let
        half =
            boardSize / 2
    in
        if (abs x) > half || (abs y) > half then
            True
        else if pathCollision ( x, y ) points then
            True
        else
            False


pathCollision ( x, y ) points =
    List.any (\( pX, pY ) -> abs (x - pX) <= 6 && abs (y - pY) <= 6) points


leftPressed pressedKeys =
    List.member ArrowLeft pressedKeys


rightPressed pressedKeys =
    List.member ArrowRight pressedKeys
