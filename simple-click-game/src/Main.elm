module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import List.Extra exposing (andThen, zip, groupsOf)


main : Program Never Model Msg
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
    , view : View
    }


type alias Game =
    { size : Int
    , moves : Int
    , board : Board
    , gameWon : Bool
    , previousStates : List Board
    }


type alias Board =
    List Cell


type alias Cell =
    { selected : Bool
    , x : Int
    , y : Int
    }


initialModel : Model
initialModel =
    { currentGame = Game 0 0 [] False []
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
            ( { model | view = Menu }, Cmd.none )


newBoard : Int -> Game
newBoard size =
    let
        all =
            List.range 0 (size - 1) |> andThen (\x -> List.range 0 (size - 1) |> andThen (\y -> [ ( x, y ) ]))

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
        , previousStates = []
        }


boolList : Int -> Random.Generator (List Bool)
boolList n =
    Random.list n Random.bool


applySelected : Game -> Cell -> Game
applySelected game selected =
    let
        history =
            game.board :: game.previousStates

        ( flippers, others ) =
            findFlippers selected game.board

        flipped =
            flipCells flippers

        board =
            List.append flipped others

        board_sorted =
            List.sortWith cellSorter board
    in
        { game | board = board_sorted, moves = (game.moves + 1), gameWon = hasWinner (board_sorted), previousStates = history }


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
            , drawGame chunked (game.gameWon == False)
            , button [ onClick ShowMenu ] [ text "menu" ]
            , div [] <| List.map (\r -> div [ shrinkStyle ] [ drawGame (groupsOf game.size r) False ]) game.previousStates
            ]


status : Game -> Html Msg
status game =
    case game.gameWon of
        False ->
            text "game on"

        True ->
            text "game over - please play again... "


drawGame : List (List Cell) -> Bool -> Html Msg
drawGame rows allowClick =
    div [] <| List.map (\r -> drawRow r allowClick) rows


drawRow : List Cell -> Bool -> Html Msg
drawRow cells allowClick =
    div [] <| List.map (\c -> drawCell c allowClick) cells


drawCell : Cell -> Bool -> Html Msg
drawCell cell allowClick =
    case ( cell.selected, allowClick ) of
        ( True, True ) ->
            span [ blockStyle, onStyle, onClick (Selected cell) ] [ text "-" ]

        ( True, False ) ->
            span [ blockStyle, onStyle ] [ text "-" ]

        ( False, True ) ->
            span [ blockStyle, offStyle, onClick (Selected cell) ] [ text "-" ]

        ( False, False ) ->
            span [ blockStyle, offStyle ] [ text "-" ]


drawHistory : List Board -> Html Msg
drawHistory boards =
    div [] []


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


shrinkStyle : Attribute msg
shrinkStyle =
    style [ ( "transform", "scale(0.5)" ) ]
