module Main exposing (..)

import Html exposing (text)
import Dict exposing (..)


--Following
-- R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
--
-- R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
--
-- R5, L5, R5, R3 leaves you 12 blocks away.
--


data : String
data =
    "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3"



--    "R8, R4, R4, R8"
--    "R2, L3"
--    "R2, R2, R2"
--    "R5, L5, R5, R3"


type Direction
    = North
    | South
    | East
    | West


type Turn
    = Left
    | Right


type alias Position =
    { x : Int
    , y : Int
    , direction : Direction
    , previous : Dict.Dict String Bool
    }


main =
    map data (Position 0 0 North Dict.empty)
        |> toString
        |> text


map : String -> Position -> Position
map instructions start =
    List.foldl (\i p -> move i p) start (String.split ", " data)


move : String -> Position -> Position
move instruction start =
    case String.uncons instruction of
        Nothing ->
            start

        Just ( f, s ) ->
            let
                t =
                    parseTurn (f)

                newDirection =
                    turn start.direction t

                steps =
                    Result.withDefault 0 (String.toInt s)
            in
                apply newDirection steps start


turn : Direction -> Turn -> Direction
turn old t =
    case t of
        Right ->
            case old of
                North ->
                    East

                East ->
                    South

                South ->
                    West

                West ->
                    North

        Left ->
            case old of
                North ->
                    West

                East ->
                    North

                South ->
                    East

                West ->
                    South


parseTurn : Char -> Turn
parseTurn s =
    case s of
        'L' ->
            Left

        _ ->
            Right


apply : Direction -> Int -> Position -> Position
apply direction steps old =
    let
        key =
            (toString old.x) ++ "," ++ (toString old.y)

        exists =
            Dict.member key old.previous

        all =
            Dict.insert key True old.previous

        _ =
            Debug.log key exists
    in
        if steps == 0 then
            old
        else
            case direction of
                North ->
                    apply direction (steps - 1) (Position (old.x + 1) old.y direction all)

                South ->
                    apply direction (steps - 1) (Position (old.x - 1) old.y direction all)

                East ->
                    apply direction (steps - 1) (Position old.x (old.y + 1) direction all)

                West ->
                    apply direction (steps - 1) (Position old.x (old.y - 1) direction all)
