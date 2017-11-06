module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Transform exposing (..)
import Time exposing (Time, second)
import Task
import List.Extra
import Keyboard.Extra exposing (Key(..))


fps =
    100


speed =
    0.11


angleSpeed =
    0.2


boardSize =
    800


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { tickTime : Time
    , initTime : Time
    , snake : Snake
    , pressedKeys : List Key
    }


type alias Snake =
    { points : List ( Float, Float )
    , angle : Float
    , state : SnakeState
    }


type SnakeState
    = Running
    | GameOver


init : ( Model, Cmd Msg )
init =
    ( initModel, Task.perform InitTime Time.now )


initModel : Model
initModel =
    { tickTime = 0
    , initTime = 0
    , snake =
        { points = []
        , angle = 0
        , state = Running
        }
    , pressedKeys = []
    }



-- UPDATE


type Msg
    = Tick Time
    | InitTime Time
    | KeyboardMsg Keyboard.Extra.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                deltaTime =
                    if model.tickTime == 0 then
                        1
                    else
                        newTime - model.tickTime
            in
                ( { model
                    | tickTime = newTime
                    , snake = updateSnake model deltaTime
                  }
                , Cmd.none
                )

        InitTime initTime ->
            ( { model | initTime = initTime }, Cmd.none )

        KeyboardMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys }, Cmd.none )


updateSnake model deltaTime =
    case model.snake.state of
        GameOver ->
            model.snake

        Running ->
            updateSnakePoision model deltaTime


updateSnakePoision model deltaTime =
    let
        ( lastX, lastY ) =
            List.Extra.last model.snake.points |> Maybe.withDefault ( 0, 0 )

        angle =
            model.snake.angle + (angleSpeed * getDirection model.pressedKeys * deltaTime)

        x =
            lastX + (speed * cos (angle * pi / 180) * deltaTime)

        y =
            lastY + (speed * sin (angle * pi / 180) * deltaTime)
    in
        { points = List.append model.snake.points [ ( x, y ) ]
        , angle = angle
        , state = updateSnakeState ( x, y ) model.snake.points
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
        else
            False


leftPressed pressedKeys =
    List.member ArrowLeft pressedKeys


rightPressed pressedKeys =
    List.member ArrowRight pressedKeys



-- View


view model =
    div [ class "board" ] [ toHtml (board model) ]


board model =
    collage boardSize boardSize [ makePath model.snake.points ]


makePath points =
    path points |> traced (lineStyle blue)


lineStyle color =
    { defaultLine
        | width = 10
        , color = color
        , cap = Round
        , join = Smooth
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (second / fps) Tick
        ]
