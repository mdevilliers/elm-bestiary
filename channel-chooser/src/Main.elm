module Main exposing (..)

import Html exposing (..)
import Html.App as Html

import Http

import Json.Decode exposing (..)
import Json.Encode

import Task

--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {
        channels : List Channel
    }


initialModel : Model
initialModel =
    { channels = [] }


init : ( Model, Cmd Msg )
init =
    ( initialModel, loadChannels )


channelData : String
channelData =
    """
{
    "channels": [
    {
        "id": "bikes",
        "value": 15,
        "recorded_at": "2016-10-11T09:46:01Z",
        "units": "bikes"
    },
    {
        "id": "open_or_total_docks",
        "value": 16,
        "recorded_at": "2016-10-11T09:46:01Z",
        "units": "bikes"
    },
    {
        "id": "spaces",
        "value": 0,
        "recorded_at": "2016-10-11T09:46:01Z",
        "units": "spaces"
    }]
}
"""

type alias Channel = 
    {
        id : String
        ,value : Int
        ,recordedAt : String
        ,units : String
    }

type alias Metadata =
    { 
        channels : List Channel
    }


-- UPDATE


type Msg
    = NoOp
    | LoadingFailed String
    | LoadingSuccess Metadata

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        LoadingFailed error ->
            (model, Cmd.none)
        LoadingSuccess metadata ->
            ( Model metadata.channels, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [
        text (toString model)
        , div[] <| List.map drawChannel model.channels
        ]

drawChannel : Channel -> Html Msg
drawChannel channel = 
    div[] [text channel.id, text ":",  text (toString channel.value) ]


loadChannels : Cmd Msg
loadChannels  = 
    Json.Decode.decodeString metadataDecoder channelData 
        |> Task.fromResult
        |> Task.perform LoadingFailed LoadingSuccess

metadataDecoder : Json.Decode.Decoder Metadata
metadataDecoder = 
    Json.Decode.object1 Metadata
        ("channels" := Json.Decode.list channelDecoder)

channelDecoder : Json.Decode.Decoder Channel
channelDecoder =
    Json.Decode.object4 Channel
        ("id" := Json.Decode.string)
        ("value" := Json.Decode.int)
        ("recorded_at" := Json.Decode.string)
        ("units" := Json.Decode.string)
