import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Http
import Json.Decode exposing ((:=)) 
import JsonApi
import JsonApi.Decode
import JsonApi.Resources
import JsonApi.Documents
import Task exposing (..)

{- 

main =
    Html.program 
    {init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- update
type Msg
  = NoOp

init : (Thing, Cmd Msg)
init =
    (Thing "none" "none", getThingResource "xxx")

update : Msg -> Thing -> (Thing, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.None)

-- view

view : Thing -> Html Msg
view model =
    div[]
     [ h2 [] [text model.title] ]

subscriptions : Thing -> Sub Msg
subscriptions model =
  Sub.none

-}

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
