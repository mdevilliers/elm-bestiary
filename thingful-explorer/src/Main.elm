module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import Dict

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
    | Navigate String
    | FetchSucceed Search.Resource
    | FetchFail Http.Error


init : ( Model , Cmd Msg)
init =
    (Model Nothing , loadSearchResults "https://api.thingful.net/things?geo-lat=51.52909&geo-long=-0.064632&geo-radius=25000&limit=20&sort=distance")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate url ->
            (model, loadSearchResults url)
        DoSearch ->
            (model, Cmd.none)
        FetchSucceed resource ->
            ( Model (Just resource), Cmd.none )
        FetchFail error ->
            let 
                _ = Debug.log "error :" error
            in
            ( model, Cmd.none )



view : Model -> Html Msg
view model =
    let 
        _ = Debug.log "model " 
    in
    div[]
    [
   --     button [onClick DoSearch] [text "Search"]
    --    ,
          (maybeResource model.results)
    ]

maybeResource : Maybe Search.Resource -> Html Msg
maybeResource resources =
    case resources of
        Just resource -> viewResource resource
        Nothing -> text ""

viewResource : Search.Resource -> Html Msg
viewResource resource =
    div[] [
        addPreviousLink resource.links
        , addNextLink resource.links
        , mapThings resource.data
    ]

-- yuck
addNextLink : Search.Links -> Html Msg
addNextLink links =
    case Dict.get "next" links of
        Just v -> addLink "next" v
        Nothing -> text ""

addPreviousLink : Search.Links -> Html Msg
addPreviousLink links =
    case Dict.get "prev" links of
        Just v -> addLink "prev" v
        Nothing -> text ""

addLink : String ->  String -> Html Msg
addLink title url =
    div[][
    a [href "#", onClick (Navigate url)] [text title]
    ]

mapThings : List Search.Thing -> Html Msg
mapThings things =
    div [] <| List.map viewThing things

viewThing : Search.Thing -> Html Msg
viewThing thing = 
    div[][
        h2 [] [text thing.id]
        , div[] [text thing.title]
        , viewMaybe thing.description
        , viewMaybe thing.longitude
        , viewMaybe thing.latitude

    ]

viewMaybe : Maybe a -> Html Msg
viewMaybe x =
    case x of
        Just y -> div[] [text (toString y)]
        Nothing -> text ""

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

loadSearchResults : String -> Cmd Msg
loadSearchResults url  =
    Task.perform FetchFail FetchSucceed (Http.get Search.resourceDecoder url)
