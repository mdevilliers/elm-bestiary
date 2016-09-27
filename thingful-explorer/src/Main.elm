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
    { results : Maybe Search.Resource
    }


type Msg = 
    DoSearch
    | FetchSucceed Search.Resource
    | FetchFail Http.Error


init : ( Model , Cmd Msg)
init =
    (Model Nothing , loadSearchResults)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoSearch ->
            (model, loadSearchResults)
        FetchSucceed resource ->
            let 
                _ = Debug.log "got it :" resource
            in
            ( Model (Just resource), Cmd.none )
        FetchFail _ ->
            ( model, Cmd.none )



view : Model -> Html Msg
view model =
    div[]
    [
        button [onClick DoSearch] [text "Search"]
        , (maybeResource model.results)
    ]

maybeResource : Maybe Search.Resource -> Html Msg
maybeResource resources =
    case resources of
        Just resource -> viewResource resource
        Nothing -> text ""

viewResource : Search.Resource -> Html Msg
viewResource resource =
    div[] [
        addLinks resource.links
        , mapThings resource.data
    ]

addLinks : Search.Links -> Html Msg
addLinks links =
    div[][ 
    a [ href "#" ] [ text "previous" ]
    , a [ href "#" ] [ text "next" ]
    ]


mapThings : List Search.Thing -> Html Msg
mapThings things =
    div [] <| List.map viewThing things

viewThing : Search.Thing -> Html Msg
viewThing thing = 
    div[][
        h2 [] [text thing.id]
        , div[] [text thing.title]
        , div[] [text thing.description]
        , viewMaybe thing.longitude
        , viewMaybe thing.latitude

    ]

viewMaybe : Maybe Float -> Html Msg
viewMaybe x =
    case x of
        Just y -> div[] [text (toString y)]
        Nothing -> text ""

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

loadSearchResults :  Cmd Msg
loadSearchResults =
  let
    url =
      "https://api.thingful.net/things?q=bikes&limit=20&sort=score"
  in
    Task.perform FetchFail FetchSucceed (Http.get Search.resourceDecoder url)

