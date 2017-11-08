module Api exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Player =
    { name : String
    , color : String
    }


decodePlayers : String -> List Player
decodePlayers str =
    Decode.decodeString (Decode.list playerDecoder) str |> Result.withDefault []


playerDecoder : Decode.Decoder Player
playerDecoder =
    decode Player
        |> required "name" Decode.string
        |> required "color" Decode.string


registerCommand model =
    (Player "Matthias" "red")


responseCode message =
    Decode.decodeString (Decode.field "response" Decode.string) message |> Result.withDefault ""


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
