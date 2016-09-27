module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task

import Search



main = 
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
    { s : String
    }


type Msg = 
    DoSearch
    | FetchSucceed Search.Resource
    | FetchFail Http.Error


init : ( Model , Cmd Msg)
init =
    (Model "" , loadSearchResults)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoSearch ->
            (model, loadSearchResults)
        FetchSucceed resource ->
            let 
                _ = Debug.log "got it :" resource
            in
            ( model, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )



view : Model -> Html Msg
view model =
    div[]
    [
        button [onClick DoSearch] [text "Search"]
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

loadSearchResults :  Cmd Msg
loadSearchResults =
  let
    url =
      "https://api.thingful.net/things?geobound-maxlat=51.52909&geobound-maxlong=-0.064632&geobound-minlat=51.511104&geobound-minlong=-0.093544&limit=2&sort=score"
  in
    Task.perform FetchFail FetchSucceed (Http.get Search.resourceDecoder url)

