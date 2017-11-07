module ScoreBoard exposing (viewScoreBoard, updateScoreBoard, Score)

import Color exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import Color.Convert exposing (..)


type alias Score =
    { name : String
    , color : Color
    , score : Int
    }


updateScoreBoard scoreBoard snakes =
    List.map (\snake -> Score snake.name snake.color (getScore snakes snake scoreBoard)) snakes


getScore snakes snake scoreBoard =
    let
        currentScore =
            List.filter (\s -> s.name == snake.name) scoreBoard |> List.map .score |> List.sum
    in
        currentScore + (rankToPoints snakes snake.rank)


rankToPoints snakes rank =
    (List.length snakes) - rank



-- View


viewScoreBoard scoreBoard =
    div [ class "scoreboard" ]
        [ h3 [] [ Html.text "Scoreboard" ]
        , table [] (List.sortBy .score scoreBoard |> List.reverse |> List.map viewScore)
        ]


viewScore score =
    tr []
        [ td [ style [ ( "background", (colorToHex score.color) ), ( "width", "5px" ) ] ] []
        , td [] []
        , td [] [ Html.text score.name ]
        , td [] [ Html.text (toString score.score) ]
        ]
