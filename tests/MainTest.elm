module MainTest exposing (..)

import Main exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Main Module"
        [ test "Test Collision Right" <| \_ -> testCollision
        , test "Test Collision Left" <| \_ -> testCollisionLeft
        ]


testCollision =
    let
        points =
            [ ( 0, 0 ), ( 0, 10 ) ]
    in
        Expect.equal True (collision ( 401, 2 ) points)


testCollisionLeft =
    let
        points =
            [ ( 0, 0 ), ( 0, 10 ) ]
    in
        Expect.equal True (collision ( -401, 2 ) points)
