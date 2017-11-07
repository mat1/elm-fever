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
    , scoreBoard : List Score
    }


type alias Score =
    { name : String
    , color : Color
    , score : Int
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
        , initSnake "Beni" green CharA CharD
        ]
    , pressedKeys = []
    , state = WaitForStart
    , scoreBoard = []
    }


initSnake : String -> Color -> Key -> Key -> Snake
initSnake name color left right =
    { points = []
    , angle = 0
    , state = Running
    , name = name
    , left = left
    , right = right
    , color = color
    , rank = 0
    }



-- UPDATE


type Msg
    = Tick Time
    | KeyboardMsg Keyboard.Extra.Msg
    | RandomInit (List StartPosition)
    | StartNextRound Time
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

                finishRound =
                    (List.filter (\s -> s.state == Running) model.snakes |> List.length) <= 1
            in
                if finishRound then
                    ( { model
                        | state = FinishRound
                        , scoreBoard = updateScoreBoard model.scoreBoard model.snakes
                      }
                    , Cmd.none
                    )
                else
                    ( { model
                        | tickTime = newTime
                        , snakes = updateSnakes model.snakes model.pressedKeys deltaTime
                      }
                    , Cmd.none
                    )

        StartNextRound time ->
            startRound model

        Start ->
            startRound model

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
                ( { model | snakes = snakes, state = RoundRunning, tickTime = 0 }, Cmd.none )


startRound model =
    ( { model | state = InitRound, pressedKeys = [] }, generateStartPosisions model )


generateStartPosisions model =
    let
        numberOfSnakes =
            List.length model.snakes
    in
        Random.generate RandomInit (Random.list numberOfSnakes (Random.map2 StartPosition (Random.float -300 300) (Random.float 0 360)))


updateScoreBoard scoreBoard snakes =
    List.map (\snake -> Score snake.name snake.color (getScore snake scoreBoard)) snakes


getScore snake scoreBoard =
    let
        currentScore =
            List.filter (\s -> s.name == snake.name) scoreBoard |> List.map .score |> List.sum
    in
        currentScore + (rankToPoints snake.rank)


rankToPoints rank =
    (numberOfSnakes - rank) * 10



-- View


view model =
    div []
        [ div [ class "board" ] [ toHtml (board model) ]
        , button [ onClick Start ] [ Html.text "Start" ]
        , div [] [ Html.text (toString model.scoreBoard) ]
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

        FinishRound ->
            Time.every (second * 3) StartNextRound

        _ ->
            Sub.none
