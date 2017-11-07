module RandomBot exposing (..)

import SnakeModel exposing (..)


getDirection : List Snake -> Snake -> Float
getDirection snakes snake =
    ((List.length snake.points) % 3 - 1) |> toFloat
