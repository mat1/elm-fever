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
import Random
import Snake exposing (..)
import Constants exposing (..)
import List.Extra


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
    | InitRound
    | RoundRunning
    | FinishRound
    | Finish


type alias StartPosition =
    { point : Float
    , angle : Float
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { tickTime = 0
    , snakes =
        [ initSnake "Matthias" blue ArrowLeft ArrowRight
        , initSnake "Blub" red CharA CharD
        ]
    , pressedKeys = []
    , state = WaitForStart
    }


initSnake name color left right =
    { points = []
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
    | KeyboardMsg Keyboard.Extra.Msg
    | Start
    | RandomInit (List StartPosition)


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

                finishRound =
                    (List.filter (\s -> s.state == Running) model.snakes |> List.length) <= 1
            in
                ( { model
                    | tickTime = newTime
                    , snakes = updateSnakes model.snakes model.pressedKeys deltaTime
                    , state =
                        if finishRound then
                            FinishRound
                        else
                            model.state
                  }
                , Cmd.none
                )

        Start ->
            ( { model | state = InitRound }, generateStartPosisions )

        KeyboardMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys }, Cmd.none )

        RandomInit randomPositions ->
            let
                startPositions =
                    List.Extra.zip randomPositions model.snakes

                snakes =
                    List.map
                        (\( startPosition, snake ) ->
                            { snake
                                | points = [ ( startPosition.point, startPosition.point ) ]
                                , angle = startPosition.angle
                                , state = Running
                            }
                        )
                        startPositions
            in
                ( { model | snakes = snakes, state = RoundRunning }, Cmd.none )


generateStartPosisions =
    Random.generate RandomInit (Random.list 2 (Random.map2 StartPosition (Random.float -300 300) (Random.float 0 360)))



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
        RoundRunning ->
            Sub.batch
                [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
                , Time.every (second / fps) Tick
                ]

        _ ->
            Sub.none
