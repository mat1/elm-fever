module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Time exposing (Time, second)
import Task
import Keyboard.Extra exposing (Key(..))
import Random
import List.Extra
import WebSocket
import Json.Encode as Encode
import Json.Decode as Decode
import Snake exposing (..)
import Constants exposing (..)
import ScoreBoard exposing (..)


snakes =
    [ initSnake "Matthias" blue ArrowLeft ArrowRight
    , initSnake "Test" red CharA CharD
    ]


numberOfSnakes =
    List.length snakes


serverUrl =
    "ws://localhost:8080/"


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


type alias Player =
    { name : String
    , color : String
    }


type alias RegisterPlayer =
    { command : String
    , player : Player
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
    }



-- UPDATE


type Msg
    = Tick Time
    | KeyboardMsg Keyboard.Extra.Msg
    | RandomInit (List StartPosition)
    | StartNextRound Time
    | Start
    | Register
    | NewMessage String


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

        Register ->
            ( model, WebSocket.send (serverUrl) (registerToJson <| RegisterPlayer "REGISTER" (Player "Matthias" "red")) )

        NewMessage str ->
            ( { model | message = str }, Cmd.none )

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
    Random.generate RandomInit (Random.list numberOfSnakes (Random.map2 StartPosition (Random.float -300 300) (Random.float 0 360)))


registerToJson : RegisterPlayer -> String
registerToJson register =
    let
        jsonValue =
            Encode.object
                [ ( "name", Encode.string register.command )
                , ( "player"
                  , Encode.object
                        [ ( "name", Encode.string register.player.name )
                        , ( "color", Encode.string register.player.color )
                        ]
                  )
                ]
    in
        (Encode.encode 0 jsonValue)



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
                ]

        FinishRound ->
            Time.every (second * 3) StartNextRound

        _ ->
            WebSocket.listen serverUrl NewMessage
