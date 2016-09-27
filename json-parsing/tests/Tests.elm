module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Array
import Search exposing (..)


all : Test
all =
    describe "foo"
        [ describe "xx"
            [ test "one" <|
                \() ->
                    let
                        t =
                            True
                    in
                        Expect.true "xx" t
            , test "two" <|
                \() -> Expect.false "xx" False
            ]
        ]
