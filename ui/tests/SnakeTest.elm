module SnakeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Snake exposing (..)


suite : Test
suite =
    describe "Snake Module"
        [ test "Test Collision Right" <| \_ -> testCollision
        , test "Test Collision Left" <| \_ -> testCollisionLeft
        , test "Test Collision With Path" <| \_ -> testCollisionWithPath
        , test "Test Collision Near Path" <| \_ -> testCollisionWithNearPath
        ]


testCollision =
    Expect.equal True (collision ( 401, 2 ) [])


testCollisionLeft =
    Expect.equal True (collision ( -401, 2 ) [])


testCollisionWithPath =
    let
        points =
            [ ( 0, 0 ), ( 3, 10 ) ]
    in
        Expect.equal True (collision ( 3, 10 ) points)


testCollisionWithNearPath =
    let
        points =
            [ ( 0, 0 ), ( 3, 10 ) ]
    in
        Expect.equal True (collision ( 2, 10 ) points)
