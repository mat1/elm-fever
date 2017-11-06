module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Time exposing (Time, second)
import Task
import Keyboard.Extra exposing (Key(..))
import Snake exposing (..)
import Constants exposing (..)


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
    , snakes : List Snake
    , pressedKeys : List Key
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Task.perform InitTime Time.now )


initModel : Model
initModel =
    { tickTime = 0
    , initTime = 0
    , snakes =
        [ initSnake "Matthias" blue ArrowLeft ArrowRight ( 30, 30 )
        , initSnake "Blub" red CharA CharD ( -40, -40 )
        ]
    , pressedKeys = []
    }


initSnake name color left right startPosition =
    { points = [ startPosition ]
    , angle = 0
    , state = Running
    , name = name
    , left = left
    , right = right
    , color = color
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
                    , snakes = updateSnakes model.snakes model.pressedKeys deltaTime
                  }
                , Cmd.none
                )

        InitTime initTime ->
            ( { model | initTime = initTime }, Cmd.none )

        KeyboardMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys }, Cmd.none )



-- View


view model =
    div [ class "board" ] [ toHtml (board model) ]


board model =
    collage boardSize boardSize (drawSnakes model.snakes)


drawSnakes snakes =
    List.map drawSnake snakes


drawSnake snake =
    path snake.points |> traced (lineStyle snake.color)


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
