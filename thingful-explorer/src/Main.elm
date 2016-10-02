port module Main exposing (..)

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


type alias Query =
    { long : Float
    , lat : Float
    , radius : Float
    , limit : Int
    , sortBy : String
    }


type Msg
    = DoSearch
    | Navigate String
    | FetchSucceed Search.Resource
    | FetchFail Http.Error


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, loadSearchResults "https://api.thingful.net/things?geo-lat=40&geo-long=-74.50&geo-radius=25000&limit=20&sort=distance" )


port mapbox : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate url ->
            ( model, loadSearchResults url )

        DoSearch ->
            ( model, Cmd.none )

        FetchSucceed resource ->
            let
                model' =
                    Model (Just resource)
            in
                ( model', mapbox "hello from elm" )

        FetchFail error ->
            let
                _ =
                    Debug.log "error :" error
            in
                ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ --     button [onClick DoSearch] [text "Search"]
          --    ,
          (maybeResource model.results)
        ]


maybeResource : Maybe Search.Resource -> Html Msg
maybeResource resources =
    case resources of
        Just resource ->
            viewResource resource

        Nothing ->
            text ""


viewResource : Search.Resource -> Html Msg
viewResource resource =
    div []
        [ addPreviousLink resource.links
        , addNextLink resource.links
        , mapThings resource.data
        ]



-- yuck


addNextLink : Search.Links -> Html Msg
addNextLink links =
    case Dict.get "next" links of
        Just v ->
            addLink "next" v

        Nothing ->
            text ""


addPreviousLink : Search.Links -> Html Msg
addPreviousLink links =
    case Dict.get "prev" links of
        Just v ->
            addLink "prev" v

        Nothing ->
            text ""


addLink : String -> String -> Html Msg
addLink title url =
    div []
        [ a [ href "#", onClick (Navigate url) ] [ text title ]
        ]


mapThings : List Search.Thing -> Html Msg
mapThings things =
    div [] <| List.map viewThing things


viewThing : Search.Thing -> Html Msg
viewThing thing =
    div []
        [ h2 [] [ text thing.id ]
        , div [] [ text thing.title ]
        , viewMaybe thing.description "Description : "
        , viewMaybe thing.longitude "Longitude : "
        , viewMaybe thing.latitude "Latitude : "
        , viewMaybe thing.score "Score : "
        , viewMaybe thing.distance "Distance : "
        , div [] [ b [] [ text "Channels :" ] ]
        , viewChannels thing.channels
        ]


viewMaybe : Maybe a -> String -> Html Msg
viewMaybe property title =
    case property of
        Just y ->
            div [] [ b [] [ text title ], text (toString y) ]

        Nothing ->
            text ""


viewChannels : List Search.Channel -> Html Msg
viewChannels channels =
    div [] <| List.map viewChannel channels


viewChannel : Search.Channel -> Html Msg
viewChannel channel =
    div []
        [ div [] [ b [] [ text "id : " ], text channel.id ]
        , div [] [ b [] [ text "value : " ], text channel.value ]
        , div [] [ b [] [ text "recorded : " ], text channel.recorded ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


loadSearchResults : String -> Cmd Msg
loadSearchResults url =
    Task.perform FetchFail FetchSucceed (Http.get Search.resourceDecoder url)
