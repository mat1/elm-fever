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
    , snake : Snake
    , pressedKeys : List Key
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Task.perform InitTime Time.now )


initModel : Model
initModel =
    { tickTime = 0
    , initTime = 0
    , snake = initSnake "Matthias" ArrowLeft ArrowRight
    , pressedKeys = []
    }


initSnake : String -> Key -> Key -> Snake
initSnake name left right =
    { points = []
    , angle = 0
    , state = Running
    , name = "Matthias"
    , left = ArrowLeft
    , right = ArrowRight
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
