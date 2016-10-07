module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import List.Extra exposing (andThen, zip, groupsOf)


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
    { currentGame : Game
      ,view : View
    }


type alias Game =
    { size : Int
    , moves : Int
    , board : List Cell
    , gameWon : Bool
    }


type alias Cell =
    { selected : Bool
    , x : Int
    , y : Int
    }


initialModel : Model
initialModel =
    { currentGame = Game 0 0 [] False
    , view = Menu
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

type View
    = Menu
    | GameInPlay

-- UPDATE


type Msg
    = NoOp
    | Selected Cell
    | NewGame Int
    | ShowMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Selected cell ->
            ( { model | currentGame = applySelected model.currentGame cell }, Cmd.none )

        NewGame n ->
            ( Model (newBoard n) GameInPlay, Cmd.none )

        ShowMenu ->
            ( {model | view = Menu } , Cmd.none )


newBoard : Int -> Game
newBoard size =
    let
        all =
            [0..(size - 1)] `andThen` \x -> [0..(size - 1)] `andThen` \y -> [ ( x, y ) ]

        -- TODO : need to set seed
        seed =
            Random.initialSeed 0

        ( values, _ ) =
            Random.step (boolList (size * size)) seed

        zipped =
            zip all values

        cells =
            List.map (\( ( x, y ), b ) -> Cell b x y) zipped
    in
        { size = size
        , moves = 0
        , board = cells
        , gameWon = False
        }


boolList : Int -> Random.Generator (List Bool)
boolList n =
    Random.list n Random.bool


applySelected : Game -> Cell -> Game
applySelected game selected =
    let
        ( flippers, others ) =
            findFlippers selected game.board

        flipped =
            flipCells flippers

        board =
            List.append flipped others

        board_sorted =
            List.sortWith cellSorter board
    in
        { game | board = board_sorted, moves = (game.moves + 1), gameWon = hasWinner (board_sorted) }


hasWinner : List Cell -> Bool
hasWinner cells =
    List.all (\a -> a.selected) cells || List.all (\a -> a.selected == False) cells


flipCells : List Cell -> List Cell
flipCells cells =
    List.map flipCell cells


flipCell : Cell -> Cell
flipCell cell =
    case cell.selected of
        False ->
            { cell | selected = True }

        True ->
            { cell | selected = False }


findFlippers : Cell -> List Cell -> ( List Cell, List Cell )
findFlippers cell board =
    List.partition (\a -> (willFlip cell a)) board


cellSorter : Cell -> Cell -> Order
cellSorter a b =
    let
        akey =
            (toString a.x) ++ (toString a.y)

        bkey =
            (toString b.x) ++ (toString b.y)
    in
        compare akey bkey


willFlip : Cell -> Cell -> Bool
willFlip cell potential =
    let
        x =
            cell.x

        y =
            cell.y

        xleft =
            x - 1

        xright =
            x + 1

        yup =
            y - 1

        ydown =
            y + 1
    in
        if cell.x == potential.x && cell.y == potential.y then
            True
        else if potential.x == xleft && potential.y == y then
            True
        else if potential.x == xright && potential.y == y then
            True
        else if potential.x == x && potential.y == yup then
            True
        else if potential.x == x && potential.y == ydown then
            True
        else
            False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.view of
        Menu ->
                showMenu
        GameInPlay ->
                showRunningGame model.currentGame

showMenu : Html Msg
showMenu =
    div []
        [ button [ onClick (NewGame 3) ] [ text "3" ]
        , button [ onClick (NewGame 5) ] [ text "5" ]
        , button [ onClick (NewGame 10) ] [ text "10" ]
    ]

showRunningGame : Game -> Html Msg
showRunningGame game =
    let
        chunked =
            groupsOf game.size game.board
    in
        div []
            [ status game
            , text (toString game.moves)
            , drawGame game chunked
            , button [ onClick ShowMenu ] [ text "menu" ]
            ]

status : Game ->Html Msg
status game =
    case game.gameWon  of
        False -> text "game on"
        True -> text "game over - please play again... "


drawGame : Game -> List (List Cell) -> Html Msg
drawGame game rows =
    div [] <| List.map (\r -> drawRow game r ) rows


drawRow : Game -> List Cell -> Html Msg
drawRow game cells =
    div [] <| List.map (\c -> drawCell game c) cells


drawCell : Game -> Cell -> Html Msg
drawCell game cell =
    case cell.selected of
        True ->
            case game.gameWon of
                False ->
                     span [ blockStyle, onStyle ,  onClick (Selected cell) ] [ text "-" ]
                True ->
                    span [ blockStyle, onStyle ] [ text "-" ]
        False ->
            case game.gameWon of
                False ->
                   span [ blockStyle, offStyle ,  onClick (Selected cell) ] [ text "-" ]
                True ->
                    span [ blockStyle, offStyle ] [ text "-" ]

blockStyle : Attribute msg
blockStyle =
    style
        [ ( "display", "inline-block" )
        , ( "width", "100px" )
        , ( "height", "100px" )
        ]


onStyle : Attribute msg
onStyle =
    style
        [ ( "backgroundColor", "red" )
        ]


offStyle : Attribute msg
offStyle =
    style
        [ ( "backgroundColor", "green" )
        ]
