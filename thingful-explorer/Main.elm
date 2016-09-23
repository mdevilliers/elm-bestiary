
import Http
import Json.Decode exposing ((:=)) 
import JsonApi
import JsonApi.Decode
import JsonApi.Resources
import JsonApi.Documents
import Task exposing (..)

import Html exposing (Html, button, div, text)
import Html.App as Html
import Html.Events exposing (onClick)


main = text "hello"

type alias Model = 
    {
    s : String
    }

type Msg = FetchSucceded String
    | FetchFailed Http.Error


init : Model
init =
    (Model "empty" )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    FetchSucceded newUrl ->
      (model, Cmd.none)

    FetchFailed _ ->
      (model, Cmd.none)


--   List Thing

type alias Thing = 
    { uid : String
    , title : String
    }

thingDecoder : Json.Decode.Decoder Thing
thingDecoder =
    Json.Decode.object2 Thing
      ("uid" := Json.Decode.string)
      ("title" := Json.Decode.string)

getThingResource : String -> Task Http.Error (JsonApi.Document)
getThingResource query =
    Http.get JsonApi.Decode.document ("https://api.thingful.net/things?geobound-maxlat=51.52909&geobound-maxlong=-0.064632&geobound-minlat=51.511104&geobound-minlong=-0.093544&limit=2&sort=score")

doSearch : JsonApi.Document -> Result String Thing
doSearch doc =
      JsonApi.Documents.primaryResource doc
          `Result.andThen` (JsonApi.Resources.attributes thingDecoder)
