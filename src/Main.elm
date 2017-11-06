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
    20


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
    , points : List ( Float, Float )
    , pressedKeys : List Key
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 0 [ ( 0, 0 ), ( 0, 10 ) ] [], Task.perform InitTime Time.now )



-- UPDATE


type Msg
    = Tick Time
    | InitTime Time
    | KeyboardMsg Keyboard.Extra.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model
                | tickTime = newTime - model.initTime
                , points = generateNextPoints model
              }
            , Cmd.none
            )

        InitTime initTime ->
            ( { model | initTime = initTime }, Cmd.none )

        KeyboardMsg keyMsg ->
            ( { model
                | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys
              }
            , Cmd.none
            )


generateNextPoints model =
    let
        ( lastX, lastY ) =
            List.Extra.last model.points |> Maybe.withDefault ( 0, 0 )

        x =
            if leftPressed model.pressedKeys then
                -2
            else if rightPressed model.pressedKeys then
                2
            else
                0

        ( newX, newY ) =
            ( lastX + x, lastY + 2 )
    in
        List.append model.points [ ( newX, newY ) ]


leftPressed pressedKeys =
    List.member ArrowLeft pressedKeys


rightPressed pressedKeys =
    List.member ArrowRight pressedKeys


view model =
    div [ class "board" ] [ toHtml (board model) ]


boardSize =
    800


board model =
    collage boardSize boardSize [ makePath model.points, makePath (points 10) ]


makePath points =
    path points |> traced (lineStyle blue)



-- make the pth point


mkPoint : Float -> Float -> ( Float, Float )
mkPoint t p =
    ( 10 * p / 3
    , 10 * sin t * sin (p / 3)
    )



-- make a list of points for each time in the animation


points : Float -> List ( Float, Float )
points t =
    List.map (mkPoint t) <|
        List.map toFloat <|
            List.range 0 5


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
