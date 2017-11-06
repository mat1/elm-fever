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
    , name : String
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
        , name = "Matthias"
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
                    , snake = updateSnake model.snake model.pressedKeys deltaTime
                  }
                , Cmd.none
                )

        InitTime initTime ->
            ( { model | initTime = initTime }, Cmd.none )

        KeyboardMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys }, Cmd.none )


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
