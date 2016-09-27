module Search exposing (..)

import Json.Decode exposing ((:=))
import Json.Encode
import Dict

type alias Resource =
    { links : Links
    , data : List Thing
    }


type alias Links =
    Dict.Dict String String

type alias Thing =
    { id : String
    , title : String
    , description : Maybe String
    , longitude : Maybe Float
    , latitude : Maybe Float
    , score : Maybe Float
    , distance : Maybe Float
    }


decodeThing : String -> Result String Thing
decodeThing payload =
    Json.Decode.decodeString thingDecoder payload


decodeResource : String -> Result String Resource
decodeResource payload =
    Json.Decode.decodeString resourceDecoder payload


resourceDecoder : Json.Decode.Decoder Resource
resourceDecoder =
    Json.Decode.object2 Resource
        ("links" := Json.Decode.dict Json.Decode.string)
        ("data" := Json.Decode.list thingDecoder)


thingDecoder : Json.Decode.Decoder Thing
thingDecoder =
    Json.Decode.object7 Thing
        ("id" := Json.Decode.string)
        (Json.Decode.at [ "attributes", "title" ] Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.at [ "attributes", "description" ] Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.at [ "attributes", "longitude" ] Json.Decode.float))
        (Json.Decode.maybe (Json.Decode.at [ "attributes", "latitude" ] Json.Decode.float))
        (Json.Decode.maybe (Json.Decode.at [ "attributes", "score" ] Json.Decode.float))
        (Json.Decode.maybe (Json.Decode.at [ "attributes", "distance" ] Json.Decode.float))
