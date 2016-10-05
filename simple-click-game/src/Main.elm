module Main exposing (..)

import Html exposing (..)
import Html.App as Html


import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


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
       currentGame : Maybe Game
    }


type alias Game =
    {
       size : Int
       ,moves : Int
       ,board : List Cell
       ,gameWon : Bool
    }

type alias Cell =
    {
        selected : Bool
        , x : Int
        , y : Int
    }

initialModel : Model
initialModel =
    {
        currentGame =Nothing
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Selected Cell
    | NewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Selected cell ->
            case model.currentGame of
                Just game ->( { model | currentGame = Just (applySelected game cell )} , Cmd.none)
                Nothing -> (model, Cmd.none)
        NewGame -> 
            ( Model (Just (newBoard 3)), Cmd.none)


newBoard : Int -> Game 
newBoard size =
    { 
        size = size
        ,moves = 0
        ,board = [ 
             Cell False 0 0, Cell False 0 1, Cell False 0 2 
            , Cell False 1 0, Cell True 1 1, Cell False 1 2 
            , Cell False 2 0, Cell False 2 1, Cell False 2 2 
        ]
        ,gameWon = False
    }


applySelected : Game -> Cell -> Game
applySelected game selected =
    let
        (flippers, others) = findFlippers selected game.board
        --_ = Debug.log "flippers" (toString flippers)
        --_ = Debug.log "others" (toString others)
        flipped = flipCells flippers
        --_ = Debug.log "all flipped" ( toString flipped)
        board = List.append flipped others
        --_ = Debug.log "new board" (toString board)
        board_sorted = List.sortWith cellSorter board
        --_ = Debug.log "board sorted" (toString board_sorted)
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
        False -> Cell True cell.x cell.y
        True -> Cell False cell.x cell.y

findFlippers : Cell -> List Cell -> (List Cell, List Cell) 
findFlippers cell board =
        List.partition (\a -> (willFlip cell a)) board

cellSorter : Cell -> Cell -> Order
cellSorter a b =
    let
        akey = (toString a.x) ++ (toString a.y)
        bkey = (toString b.x) ++ (toString b.y)
    in
    compare akey bkey

willFlip : Cell -> Cell -> Bool
willFlip cell potential =
    let
        x = cell.x
        y = cell.y
        xleft = x - 1
        xright = x + 1
        yup = y - 1
        ydown = y + 1
    in
    if cell.x == potential.x && cell.y == potential.y
    then True
    else if potential.x == xleft && potential.y == y
    then True
    else if potential.x == xright && potential.y == y
    then True
    else if potential.x == x && potential.y == yup
    then True
    else if potential.x == x && potential.y == ydown
    then True
    else False

chunksOfLeft : Int -> List a -> List (List a)
chunksOfLeft k xs =
  let len = List.length xs
  in  if len > k
      then List.take k xs :: chunksOfLeft k (List.drop k xs)
      else [xs]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.currentGame of
        Just game -> showRunningGame game
        Nothing -> div[] 
                    [ button [ onClick NewGame] [text "start game" ]
                    ]


showRunningGame : Game -> Html Msg
showRunningGame game =
    let
        chunked = chunksOfLeft game.size game.board
    in
    div []
        [ text "game on!"
        , text ( toString game.moves)
        , text (toString game.gameWon)
        , drawGame chunked
        ]


--decideScreen : Model -> Html Msg

drawGame : List (List Cell) -> Html Msg
drawGame rows =
    div[] <| List.map drawRow rows

drawRow : List Cell -> Html Msg
drawRow cells =
    div[] <| List.map drawCell cells 

drawCell : Cell -> Html Msg
drawCell cell =
    case cell.selected of
        True -> span[ selectedStyle, onClick (Selected cell) ] [ text (toString cell) ]
        False -> span[ onClick (Selected cell) ] [ text (toString cell) ]

selectedStyle : Attribute msg
selectedStyle =
  style
    [ ("backgroundColor", "red")
    ]
