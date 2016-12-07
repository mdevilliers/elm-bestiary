module Main exposing (..)

import Html exposing (text)
import MD5 exposing (..)


main =
    partone
        |> toString
        |> text


partone : String
partone =
    let
        a =
            hash 0 data

        b =
            hash (a.iteration + 1) data

        c =
            hash (b.iteration + 1) data

        d =
            hash (c.iteration + 1) data

        e =
            hash (d.iteration + 1) data

        f =
            hash (e.iteration + 1) data

        g =
            hash (f.iteration + 1) data

        h =
            hash (g.iteration + 1) data
    in
        a.letter ++ b.letter ++ c.letter ++ d.letter ++ e.letter ++ f.letter ++ g.letter ++ h.letter


type alias State =
    { iteration : Int
    , letter : String
    }


hash : Int -> String -> State
hash count str =
    let
        hashed =
            MD5.hex (str ++ (toString count))
    in
        case String.left 5 hashed of
            "00000" ->
                State count (String.slice 5 6 hashed)

            _ ->
                hash (count + 1) str


data : String
data =
    "ffykfhsq"
