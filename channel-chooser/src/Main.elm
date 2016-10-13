module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Http
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Task
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Dict


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
    { currentEntitlement : Maybe Entitlements
    , metadata : Metadata
    }


initialModel : Model
initialModel =
    { currentEntitlement = Nothing
    , metadata = Metadata [] (Location 0.0 0.0 "-") (Owner "-" "-")
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, loadChannels )


channelData : String
channelData =
    """
    {
    "owner" : {
        "name" : "John Smith",
        "email" : "john@smith.com"
    },
    "location" : {
        "latitude" : 1.23,
        "longitude" : 3.45,
        "address" : "SW1 London GB"
    },
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
    { id : String
    , value : Int
    , recordedAt : String
    , units : String
    }


type alias Metadata =
    { channels : Channels
    , location : Location
    , owner : Owner
    }


type alias Channels =
    List Channel


type alias Visible =
    Bool


type Group
    = GeneralPublic
    | EmergencyServices
    | Academic
    | Commercial
    | NonProfit


type alias ChannelEntitlement =
    { channel : Channel
    , visible : Visible
    }


type alias ChannelEntitlements =
    List ChannelEntitlement


type Modifier
    = ShowLocationDetails
    | ShowOwnerDetails
    | ShowChannelDetails


type alias Entitlements =
    List Entitlement


type alias Entitlement =
    { group : Group
    , modifiers : List Modifier
    , channels : ChannelEntitlements
    }


type alias Location =
    { longitude : Float
    , latitude : Float
    , address : String
    }


type alias Owner =
    { name : String
    , email : String
    }



-- UPDATE


type Msg
    = NoOp
    | LoadingFailed String
    | LoadingSuccess Metadata
    | EnableEntitlements
    | DisableEntitlements
    | AddEntitlement Group
    | RemoveEntitlement Group
    | SetModifier Entitlement Modifier
    | SetChannelVisiblity Entitlement ChannelEntitlement Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "update :" (toString msg)
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            LoadingFailed error ->
                ( model, Cmd.none )

            LoadingSuccess metadata ->
                ( { model | metadata = metadata }, Cmd.none )

            EnableEntitlements ->
                ( { model | currentEntitlement = (addEntitlement model.currentEntitlement (newEntitlement GeneralPublic model.metadata)) }, Cmd.none )

            DisableEntitlements ->
                ( { model | currentEntitlement = Nothing }, Cmd.none )

            AddEntitlement group ->
                ( { model | currentEntitlement = (addEntitlement model.currentEntitlement (newEntitlement group model.metadata)) }, Cmd.none )

            RemoveEntitlement group ->
                ( { model | currentEntitlement = (removeEntitlement model.currentEntitlement group) }, Cmd.none )

            SetModifier entitlement modifier->
                let
                    modifiers =
                        addOrRemove entitlement.modifiers modifier

                    entitlement' =
                        { entitlement | modifiers = modifiers }
                in
                    ( { model | currentEntitlement = (replaceEntitlements model.currentEntitlement entitlement') }, Cmd.none )
            SetChannelVisiblity entitlement channel visible ->
                let
                    channel' = {channel | visible = visible}
                    entitlement' = replaceChannel entitlement channel'
                in
                ( {model | currentEntitlement = (replaceEntitlements model.currentEntitlement entitlement')} , Cmd.none)


newEntitlement : Group -> Metadata -> Entitlement
newEntitlement g metadata =
    let
        channelEntitlements =
            List.map (\c -> ChannelEntitlement c False) metadata.channels
    in
        (Entitlement g [] channelEntitlements)


flipVisibility : Visible -> Visible
flipVisibility visible =
    case visible of
        True ->
            False

        False ->
            True


addOrRemove : List a -> a -> List a
addOrRemove l m =
    case List.member m l of
        True ->
            List.filter (\x -> x /= m) l

        False ->
            m :: l


replaceChannel : Entitlement -> ChannelEntitlement -> Entitlement
replaceChannel entitlement channel =
       case List.filter (\c -> c.channel.id /= channel.channel.id ) entitlement.channels of
           [] ->  {entitlement | channels = [channel]}
           x ->   {entitlement | channels = channel :: x }


removeEntitlement : Maybe Entitlements -> Group -> Maybe Entitlements
removeEntitlement entitlements group =
    case entitlements of
        Nothing ->
            entitlements

        Just x ->
            let
                x' =
                    List.filter (\e -> e.group /= group) x
            in
                case x' of
                    [] ->
                        Nothing

                    other ->
                        Just (x')


addEntitlement : Maybe Entitlements -> Entitlement -> Maybe Entitlements
addEntitlement entitlements e =
    case entitlements of
        Nothing ->
            Just [ e ]

        Just x ->
            case List.any (\t -> t.group == e.group) x of
                True ->
                    entitlements

                False ->
                    Just (sortEntitlements (e :: x))


replaceEntitlements : Maybe Entitlements -> Entitlement -> Maybe Entitlements
replaceEntitlements entitlements e =
    let
        entitlements' =
            removeEntitlement entitlements e.group
    in
        addEntitlement entitlements' e


sortEntitlements : Entitlements -> Entitlements
sortEntitlements e =
    List.sortWith entitlementSorter e


entitlementSorter : Entitlement -> Entitlement -> Order
entitlementSorter a b =
    compare (toString a.group) (toString b.group)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text (toString model)
        , div [] []
        , drawEntitlementEditor model
        ]


drawEntitlementEditor : Model -> Html Msg
drawEntitlementEditor model =
    case model.currentEntitlement of
        Nothing ->
            drawEntitlementSelector False

        Just x ->
            div []
                [ drawEntitlementSelector True
                , div [] <| List.map (\n -> drawGroupEditor n model.metadata) x
                , drawGroupSelector x
                ]


drawEntitlementSelector : Bool -> Html Msg
drawEntitlementSelector selected =
    div []
        [ fieldset []
            [ label []
                [ input [ type' "checkbox", checked selected, onClick (entitlementSelectorAction selected) ] []
                , text "Entitlements"
                ]
            ]
        ]


entitlementSelectorAction : Bool -> Msg
entitlementSelectorAction b =
    case b of
        False ->
            EnableEntitlements

        True ->
            DisableEntitlements


drawGroupEditor : Entitlement -> Metadata -> Html Msg
drawGroupEditor entitlement metadata =
    div []
        [ fieldset []
            [ text (toString entitlement.group)
            , div [] []
            , a [ href "#", onClick (RemoveEntitlement entitlement.group) ] [ text "remove" ]
            , div [] []
            , label []
                [ input [ type' "checkbox", checked (List.member ShowLocationDetails entitlement.modifiers), onClick (SetModifier entitlement ShowLocationDetails) ] []
                , text "disclose location"
                ]
            , label []
                [ input [ type' "checkbox", checked (List.member ShowOwnerDetails entitlement.modifiers), onClick (SetModifier entitlement ShowOwnerDetails) ] []
                , text "disclose your details"
                ]
            , label []
                [ input [ type' "checkbox", checked (List.member ShowChannelDetails entitlement.modifiers), onClick (SetModifier entitlement ShowChannelDetails) ] []
                , text "view channel values"
                ]
            , fieldset [] [ drawLocationView entitlement metadata.location ]
            , fieldset [] [ drawOwnerView entitlement metadata.owner ]
            , fieldset [] [ drawChannelsView entitlement ]
            ]
          -- end of fieldset
        ]


drawOwnerView : Entitlement -> Owner -> Html Msg
drawOwnerView entitlement owner =
    let
        showPrecise =
            List.member ShowOwnerDetails entitlement.modifiers
    in
        case showPrecise of
            False ->
                div [] [ text "no details disclosed" ]

            True ->
                div []
                    [ text "name : "
                    , text owner.name
                    , div [] []
                    , text "email : "
                    , text owner.email
                    ]


drawLocationView : Entitlement -> Location -> Html Msg
drawLocationView entitlement location =
    let
        showPrecise =
            List.member ShowLocationDetails entitlement.modifiers
    in
        case showPrecise of
            False ->
                div []
                    [ text location.address
                    ]

            True ->
                div []
                    [ text "address : "
                    , text location.address
                    , div [] []
                    , text "latitude : "
                    , text (toString location.latitude)
                    , div [] []
                    , text "longitude :"
                    , text (toString location.longitude)
                    ]


drawChannelsView : Entitlement -> Html Msg
drawChannelsView ent =
    let
        showValues =
            List.member ShowChannelDetails ent.modifiers
    in
        div [] <| List.map (\channel -> drawChannel ent channel showValues) ent.channels


drawChannel : Entitlement -> ChannelEntitlement -> Bool -> Html Msg
drawChannel entitlement channelEntitlement showValues =
    case showValues of
        True ->
            div [] [ text channelEntitlement.channel.id, text " : "
                   , text (toString channelEntitlement.channel.value)
                   , a[ href "#", onClick (SetChannelVisiblity entitlement channelEntitlement False )] [text "hide"]
                   ]

        False ->
            div [] [ text channelEntitlement.channel.id ]


drawGroupSelector : Entitlements -> Html Msg
drawGroupSelector entitlements =
    div []
        [ select [ onSelect AddEntitlement ] <| List.map (\( x, v ) -> option [ Html.Attributes.value (toString v) ] [ text (toString x) ]) allGroupsForDropdown
        ]


groupSelectorDecoder : Json.Decode.Decoder Group
groupSelectorDecoder =
    Json.Decode.at [ "target", "selectedIndex" ] Json.Decode.int `Json.Decode.andThen` groupInfo


groupInfo : Int -> Json.Decode.Decoder Group
groupInfo tag =
    let
        filtered =
            allGroupsForDropdown
                |> List.filter (\x -> snd (x) == tag)
                |> List.head
    in
        case filtered of
            Nothing ->
                Json.Decode.fail "!"

            Just x ->
                Json.Decode.succeed (fst x)


onSelect : (Group -> msg) -> Html.Attribute msg
onSelect msg =
    on "change" (Json.Decode.map msg groupSelectorDecoder)


allGroupsForDropdown : List ( Group, Int )
allGroupsForDropdown =
    [ ( GeneralPublic, 0 ), ( EmergencyServices, 1 ), ( Academic, 2 ), ( Commercial, 3 ), ( NonProfit, 4 ) ]


loadChannels : Cmd Msg
loadChannels =
    Json.Decode.decodeString metadataDecoder channelData
        |> Task.fromResult
        |> Task.perform LoadingFailed LoadingSuccess


metadataDecoder : Json.Decode.Decoder Metadata
metadataDecoder =
    Json.Decode.object3 Metadata
        ("channels" := Json.Decode.list channelDecoder)
        ("location" := locationDecoder)
        ("owner" := ownerDecoder)


locationDecoder : Json.Decode.Decoder Location
locationDecoder =
    Json.Decode.object3 Location
        ("longitude" := Json.Decode.float)
        ("latitude" := Json.Decode.float)
        ("address" := Json.Decode.string)


channelDecoder : Json.Decode.Decoder Channel
channelDecoder =
    Json.Decode.object4 Channel
        ("id" := Json.Decode.string)
        ("value" := Json.Decode.int)
        ("recorded_at" := Json.Decode.string)
        ("units" := Json.Decode.string)


ownerDecoder : Json.Decode.Decoder Owner
ownerDecoder =
    Json.Decode.object2 Owner
        ("email" := Json.Decode.string)
        ("name" := Json.Decode.string)
