module Main exposing (..)

import Html exposing (text)
import Dict exposing (..)



data : String
data =
--    """ULL
--RRDDD
--LURDL
--UUUUD
--"""

        """LLLUDRDLLULDUDLLRLUDURULDURRRRLLURLDLDDDULLDDUDLRDLRDUURRDUUDLLRUUDDLULURDLRDUUDUDRURULLLLDRULDDLRDDRDLRDDLURLDDUDLLUUDLRDDDDLULUUURRDLUUDDLULLURRRDULLUDUDRDRDDRLDLLRLRDDDRDLULLUULDLLLRRDDUURUURDLLDRRDDLRULDLLDRLLUDRRDLUUULDLURLLDDURRUULLLLLRLUDLDDLLLURRRDUDULRULULDDLLDLRDDDUULRLRDUURLURRUDDURRUUDUDLDDLDULDDDDDULRULRDLRLDLRDLDDLUDRDUUDLDUDUDLLLRLUUDRUDDDRDRURRLLLDLUULDUULRDLULLUURLDRRRLDRLUDRRURLDULULDRUDDULLLDRDLLULUDDDDRDRULDULRRRRDDRULDLRDU
DLDDRRDLLDUURRLULLLLRDRLUDURLDRRDURRRRUUDDUDRRDDDRRLDDLDDLURDLDRLUDULDUDUUDDDLLULRRLLUDULLLUULDDRDDUDUUDULURULULLDRLRUURDRDDLRRUDRUULLLLURRUDRDULDDRURRURLLLLLRLLLRLLUDUDLRDLULDUDLULLLUUDLLDDDUDUDLLRRDLRDLLLRRLRUDRDUDRURLUUURULLDDDDLLURDULURRLLLRLRRULLRRRLUUULLLLRLRDUURRDUURLLLDRDURRRULDDUDRRDLRLDLLRUDDLLUDRLLUUDRLLLLLLUDLURLLRUDRUUUULDLUDULLDDDDRLURUURDDDURRRLRLUDUUDURDDDDRRLRDLURDDLLULDRDDURLLURULUUUUURDUUULDRRLLURRRRRLDDUULLRULLDLDLDLRRRDRLDRUUD
RLDRRRURULDLUDLDUDLLDUUURRDUDDURULLRRDDULUUDRRRULRUURRRLUUULRDRUDRRLLRLURDLDRDRDLLUDRUULRUDRUDDRURLRLURRDDRRURUUDRRDDRURURUDUUUDUDRRLRDRUUURLLUUUDLRUUDDRDDDDLDRLRDUDDULDDLRLLRURLLURLDDLDLDDULLDDUUURDLRUDUUDLDURDDRUULDRDDRDDDUUUDRDRDDRRDRRDLRDRURDUDDLUUUDULLUULULULRDRUUDDURURDRRRRLUDLDUDURLDRURDLLUUUDLRRDRRURDDULULURLDUDDLUDLDDLLRLDULLULULURUURLDULUDLLUUDLDDULDRRDDUULLUDLDLLRDRDURDDURDDURLDDURUURLLRURURUDDURRDRLRLDDUUDUULRDLLURRRRULURULDUDUDDUDDRLLLDLURDUURUURLUULRRLDLULDDRLDDUURULURUDRD
URLDDRLLRRLDRLLRRURURURDDLRRRUUUURULRRUUDLUDRULLDLRUDDLULRUULDULURLLRLLUDDUDLURDRRRRLURULRURRURRULRRRULDLLDDLRLUDULUUUDDUDDRRDDDDUULRRLDRRULULRDUURRLDDRDULDURUDUDDLDLLURDDLDDRUDUUUDUUURDLDUDUUULLDLRDULRRRDLLURLDLLULRDDULULURLRLUULRLLLDDDUDLLDLURRRULRDUDDLULUDRUDDURULRLRUDDURLLURULLURDRULDUDLDULRRDLDURLUURRDDUDDUDRURUDDURRUUDURUULLLLDDRDDDDDULUUDDURRULLDRRLRRRRRDDRUUDDDURDRDRUDDUULDUDRRDRULUURLURLUDUDULDDRDULDLRUUDLLLRRLRDRDDUUULRDUDLUDLURRDUURDULDRLLDRDULDUDUULRLLDLRLDLUUDLRUULDUUULDLRLRLUULLLLRLRDUDRUUDURLDUDRRURLRUDRRLRDDLRDDLDDUDDDRLRLLRLUUURLURRRLULRLLDRLRDDRRDRL
DLLLLLLRLRDRUDLRLLRLDLRURRUURLDLDDDDDUDUULLLLRRLRRDUUDUDLULLRRDULUDLLULURLRULURUULRLURDUDLUDULULUUURLRUDULURULRURULURLRLDRRRRLUDLLDULLDDLLULUURRULRDURDUUDDDURRUDLLLLRLDLUDDULLDUDDURURURRRRULDULULUDDUUDRLRLLLDLLLUUUURUDUUDLDLLRLRDDUULLUURLDDLRRDRLULDLULRULDLDURLULUURRRUDLLRDLUDDULRULULUDDURDLUUURDUUURDUDURLUUDRLUDRULUDDRRDLUUDLLLRDDDDDDLDURDDLDRDLUUDRULLUDRDLDULLULDDRUUDRRLRURRUULLRLRDUUURRDRRDULDDULUUDDURLULRLRURLLRRR"""

main =
    map data (Position 5 "")
        |> toString
        |> text

type Direction
    = U
    | L
    | R
    | D

type alias Position =
    {current : Int
    ,combination : String
   }

map : String -> Position -> Position 
map instructions start =
     List.foldl (\i p -> process i p) start (String.split "\n" data)

process : String -> Position -> Position
process instructions start =
    let 
        _ = Debug.log instructions start
    in
    case String.uncons instructions of
        Nothing ->
            let
                c = start.combination ++ toString(start.current)
            in
                {start | combination = c }
        Just (f,s) ->
            let
                newPosition = move (toDirection f) start
            in
                process s newPosition

-- 1 2 3
-- 4 5 6
-- 7 8 9
move : Direction -> Position -> Position
move direction start =
    case start.current of
        1 ->
            case direction of
                U -> start
                L -> start
                R -> {start | current = 2}
                D -> {start | current = 4}
        2 ->
            case direction of
                U -> start
                L -> {start | current = 1}
                R -> {start | current = 3}
                D -> {start | current = 5}
        3 ->
            case direction of
                U -> start
                L -> {start | current = 2}
                R -> start
                D ->  {start | current = 6}
        4 ->
            case direction of
                U ->  {start | current = 1}
                L -> start
                R ->  {start | current = 5}
                D ->  {start | current = 7}
        5 ->
            case direction of
                U -> {start | current = 2}
                L ->{start | current = 4}
                R -> {start | current = 6}
                D -> {start | current = 8}
        6 ->
            case direction of
                U -> {start | current = 3}
                L -> {start | current = 5}
                R -> start
                D -> {start | current = 9}

        7 ->
            case direction of
                U -> {start | current = 4}
                L -> start
                R -> {start | current = 8}
                D -> start
        8 ->
            case direction of
                U -> {start | current = 5}
                L -> {start | current = 7}
                R -> {start | current = 9}
                D -> start
        9 ->
            case direction of
                U -> {start | current = 6}
                L -> {start | current = 8}
                R -> start
                D -> start
        _ -> start

toDirection : Char -> Direction
toDirection c =
    case c of
        'U' -> U
        'L' -> L
        'R' -> R
        'D' -> D 
        _  -> U --doesn't matter
