module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Time exposing (Time, second, millisecond)
import Task
import Keyboard.Extra exposing (Key(..))
import Random
import List.Extra
import WebSocket
import Snake exposing (..)
import SnakeModel exposing (..)
import Constants exposing (..)
import ScoreBoard exposing (..)
import Api exposing (..)


snakes =
    [ initSnake "Florian" blue ArrowLeft ArrowRight
    , initSnake "M" red CharA CharD
    ]


serverUrl =
    "ws://localhost:8080/"


playersUrl =
    serverUrl ++ "players"


gameStateUrl =
    serverUrl ++ "gameState"


movesUrl =
    serverUrl ++ "moves"


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
    , message : String
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
    , snakes = snakes
    , pressedKeys = []
    , state = WaitForStart
    , scoreBoard = initScoreBoard snakes
    , message = ""
    }


initScoreBoard snakes =
    List.map (\s -> Score s.name s.color 0) snakes


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
    , snakePlayer = Self
    , direction = Straight
    }


initSnakeOther : String -> Color -> Snake
initSnakeOther name color =
    { points = []
    , angle = 0
    , state = Running
    , name = name
    , left = CharZ
    , right = CharZ
    , color = color
    , rank = 0
    , snakePlayer = SnakeModel.Other
    , direction = Straight
    }



-- UPDATE


type Msg
    = Tick Time
    | KeyboardMsg Keyboard.Extra.Msg
    | RandomInit (List StartPosition)
    | StartNextRound Time
    | Start
    | Register
    | UpdatePlayers String
    | UpdateDirection String
    | UpdateGameState Time


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

        UpdateGameState time ->
            ( model, WebSocket.send gameStateUrl (modelToJson model) )

        UpdateDirection str ->
            let
                playerMove =
                    Debug.log "Hello: " (decodeMove str)

                snakes =
                    List.map (\s -> updateDirection s playerMove) model.snakes
            in
                ( { model | snakes = snakes }, Cmd.none )

        StartNextRound time ->
            startRound model

        Register ->
            ( model, WebSocket.send (serverUrl ++ "/register") "" )

        UpdatePlayers str ->
            let
                players =
                    decodePlayers str

                newSnakes =
                    toSnakes players

                updatedSnakes =
                    snakes ++ newSnakes
            in
                ( { model | message = str, snakes = updatedSnakes, scoreBoard = initScoreBoard updatedSnakes }, Cmd.none )

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


updateDirection snake move =
    if snake.name == move.name then
        { snake | direction = move.direction }
    else
        snake


startRound model =
    ( { model | state = InitRound, pressedKeys = [] }, generateStartPosisions model )


generateStartPosisions model =
    Random.generate RandomInit (Random.list (List.length model.snakes) (Random.map2 StartPosition (Random.float -300 300) (Random.float 0 360)))


toSnakes : List Player -> List Snake
toSnakes players =
    List.map (\p -> initSnakeOther p.name (stringToColor p.color)) players


stringToColor color =
    if color == "red" then
        red
    else if color == "orange" then
        orange
    else if color == "yellow" then
        yellow
    else if color == "green" then
        green
    else if color == "blue" then
        blue
    else
        brown



-- View


view model =
    div []
        [ div [ class "boardcontainer" ]
            [ div [ class "board" ] [ toHtml (board model) ]
            , div [ class "info" ]
                [ viewScoreBoard model.scoreBoard
                , button [ onClick Start ] [ Html.text "Start" ]
                , button [ onClick Register ] [ Html.text "Register" ]
                ]
            ]
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
                , Time.every updateGameStateInMilliseconds UpdateGameState
                , WebSocket.listen (movesUrl) UpdateDirection
                ]

        FinishRound ->
            Time.every (second * 3) StartNextRound

        _ ->
            Sub.batch
                [ WebSocket.listen (playersUrl) UpdatePlayers
                , WebSocket.listen (movesUrl) UpdateDirection
                ]
