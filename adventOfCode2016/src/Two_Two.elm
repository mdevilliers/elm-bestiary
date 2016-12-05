module Main exposing (..)

import Html exposing (text)
import Dict exposing (..)


data : String
data =
    """LLLUDRDLLULDUDLLRLUDURULDURRRRLLURLDLDDDULLDDUDLRDLRDUURRDUUDLLRUUDDLULURDLRDUUDUDRURULLLLDRULDDLRDDRDLRDDLURLDDUDLLUUDLRDDDDLULUUURRDLUUDDLULLURRRDULLUDUDRDRDDRLDLLRLRDDDRDLULLUULDLLLRRDDUURUURDLLDRRDDLRULDLLDRLLUDRRDLUUULDLURLLDDURRUULLLLLRLUDLDDLLLURRRDUDULRULULDDLLDLRDDDUULRLRDUURLURRUDDURRUUDUDLDDLDULDDDDDULRULRDLRLDLRDLDDLUDRDUUDLDUDUDLLLRLUUDRUDDDRDRURRLLLDLUULDUULRDLULLUURLDRRRLDRLUDRRURLDULULDRUDDULLLDRDLLULUDDDDRDRULDULRRRRDDRULDLRDU
DLDDRRDLLDUURRLULLLLRDRLUDURLDRRDURRRRUUDDUDRRDDDRRLDDLDDLURDLDRLUDULDUDUUDDDLLULRRLLUDULLLUULDDRDDUDUUDULURULULLDRLRUURDRDDLRRUDRUULLLLURRUDRDULDDRURRURLLLLLRLLLRLLUDUDLRDLULDUDLULLLUUDLLDDDUDUDLLRRDLRDLLLRRLRUDRDUDRURLUUURULLDDDDLLURDULURRLLLRLRRULLRRRLUUULLLLRLRDUURRDUURLLLDRDURRRULDDUDRRDLRLDLLRUDDLLUDRLLUUDRLLLLLLUDLURLLRUDRUUUULDLUDULLDDDDRLURUURDDDURRRLRLUDUUDURDDDDRRLRDLURDDLLULDRDDURLLURULUUUUURDUUULDRRLLURRRRRLDDUULLRULLDLDLDLRRRDRLDRUUD
RLDRRRURULDLUDLDUDLLDUUURRDUDDURULLRRDDULUUDRRRULRUURRRLUUULRDRUDRRLLRLURDLDRDRDLLUDRUULRUDRUDDRURLRLURRDDRRURUUDRRDDRURURUDUUUDUDRRLRDRUUURLLUUUDLRUUDDRDDDDLDRLRDUDDULDDLRLLRURLLURLDDLDLDDULLDDUUURDLRUDUUDLDURDDRUULDRDDRDDDUUUDRDRDDRRDRRDLRDRURDUDDLUUUDULLUULULULRDRUUDDURURDRRRRLUDLDUDURLDRURDLLUUUDLRRDRRURDDULULURLDUDDLUDLDDLLRLDULLULULURUURLDULUDLLUUDLDDULDRRDDUULLUDLDLLRDRDURDDURDDURLDDURUURLLRURURUDDURRDRLRLDDUUDUULRDLLURRRRULURULDUDUDDUDDRLLLDLURDUURUURLUULRRLDLULDDRLDDUURULURUDRD
URLDDRLLRRLDRLLRRURURURDDLRRRUUUURULRRUUDLUDRULLDLRUDDLULRUULDULURLLRLLUDDUDLURDRRRRLURULRURRURRULRRRULDLLDDLRLUDULUUUDDUDDRRDDDDUULRRLDRRULULRDUURRLDDRDULDURUDUDDLDLLURDDLDDRUDUUUDUUURDLDUDUUULLDLRDULRRRDLLURLDLLULRDDULULURLRLUULRLLLDDDUDLLDLURRRULRDUDDLULUDRUDDURULRLRUDDURLLURULLURDRULDUDLDULRRDLDURLUURRDDUDDUDRURUDDURRUUDURUULLLLDDRDDDDDULUUDDURRULLDRRLRRRRRDDRUUDDDURDRDRUDDUULDUDRRDRULUURLURLUDUDULDDRDULDLRUUDLLLRRLRDRDDUUULRDUDLUDLURRDUURDULDRLLDRDULDUDUULRLLDLRLDLUUDLRUULDUUULDLRLRLUULLLLRLRDUDRUUDURLDUDRRURLRUDRRLRDDLRDDLDDUDDDRLRLLRLUUURLURRRLULRLLDRLRDDRRDRL
DLLLLLLRLRDRUDLRLLRLDLRURRUURLDLDDDDDUDUULLLLRRLRRDUUDUDLULLRRDULUDLLULURLRULURUULRLURDUDLUDULULUUURLRUDULURULRURULURLRLDRRRRLUDLLDULLDDLLULUURRULRDURDUUDDDURRUDLLLLRLDLUDDULLDUDDURURURRRRULDULULUDDUUDRLRLLLDLLLUUUURUDUUDLDLLRLRDDUULLUURLDDLRRDRLULDLULRULDLDURLULUURRRUDLLRDLUDDULRULULUDDURDLUUURDUUURDUDURLUUDRLUDRULUDDRRDLUUDLLLRDDDDDDLDURDDLDRDLUUDRULLUDRDLDULLULDDRUUDRRLRURRUULLRLRDUUURRDRRDULDDULUUDDURLULRLRURLLRRR"""



--    1
--  2 3 4
--5 6 7 8 9
--  A B C
--    D


main =
    let
        structure =
            Dict.fromList
                [ ( "1", Node Nothing Nothing Nothing (Just "3") )
                , ( "2", Node Nothing Nothing (Just "3") (Just "6") )
                , ( "3", Node (Just "1") (Just "2") (Just "4") (Just "7") )
                , ( "4", Node Nothing (Just "3") Nothing (Just "8") )
                , ( "5", Node Nothing Nothing (Just "6") Nothing )
                , ( "6", Node (Just "2") (Just "5") (Just "7") (Just "A") )
                , ( "7", Node (Just "3") (Just "6") (Just "8") (Just "B") )
                , ( "8", Node (Just "4") (Just "7") (Just "9") (Just "C") )
                , ( "9", Node Nothing (Just "8") Nothing Nothing )
                , ( "A", Node (Just "6") Nothing (Just "B") Nothing )
                , ( "B", Node (Just "7") (Just "A") (Just "C") (Just "D") )
                , ( "C", Node (Just "8") (Just "B") Nothing Nothing )
                , ( "D", Node (Just "B") Nothing Nothing Nothing )
                ]
    in
        solve structure (Position "5" "") data
            |> toString
            |> text


type Direction
    = U
    | L
    | R
    | D


type alias Data =
    Dict String Node


type alias Position =
    { current : String
    , combination : String
    }


type alias Node =
    { u : Maybe String
    , l : Maybe String
    , r : Maybe String
    , d : Maybe String
    }


solve : Data -> Position -> String -> Position
solve structure start instructions =
    List.foldl (\i p -> process i p structure) start (String.split "\n" data)


process : String -> Position -> Data -> Position
process instructions start data =
    case String.uncons instructions of
        Nothing ->
            let
                c =
                    start.combination ++ toString (start.current)
            in
                { start | combination = c }

        Just ( f, s ) ->
            let
                newPosition =
                    move (toDirection f) start data
            in
                process s newPosition data


move : Direction -> Position -> Data -> Position
move direction start data =
    let
        node =
            Maybe.withDefault (Node Nothing Nothing Nothing Nothing) (Dict.get start.current data)
    in
        case direction of
            U ->
                { start | current = Maybe.withDefault start.current node.u }

            L ->
                { start | current = Maybe.withDefault start.current node.l }

            R ->
                { start | current = Maybe.withDefault start.current node.r }

            D ->
                { start | current = Maybe.withDefault start.current node.d }


toDirection : Char -> Direction
toDirection c =
    case c of
        'U' ->
            U

        'L' ->
            L

        'R' ->
            R

        'D' ->
            D

        _ ->
            U
