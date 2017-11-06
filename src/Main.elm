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
    , snakes : List Snake
    , pressedKeys : List Key
    , state : GameState
    }


type GameState
    = WaitForStart
    | StartGame
    | Run


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { tickTime = 0
    , snakes =
        [ initSnake "Matthias" blue ArrowLeft ArrowRight ( 30, 30 ) 30
        , initSnake "Blub" red CharA CharD ( -40, -40 ) -20
        ]
    , pressedKeys = []
    , state = WaitForStart
    }


initSnake name color left right startPosition angle =
    { points = [ startPosition ]
    , angle = angle
    , state = Running
    , name = name
    , left = left
    , right = right
    , color = color
    }



-- UPDATE


type Msg
    = Tick Time
    | KeyboardMsg Keyboard.Extra.Msg
    | Start


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

        Start ->
            ( { model | state = StartGame }, Cmd.none )

        KeyboardMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys }, Cmd.none )



-- View


view model =
    div []
        [ div [ class "board" ] [ toHtml (board model) ]
        , button [ onClick Start ] [ Html.text "Start" ]
        ]


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
    case model.state of
        StartGame ->
            Sub.batch
                [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
                , Time.every (second / fps) Tick
                ]

        _ ->
            Sub.none
