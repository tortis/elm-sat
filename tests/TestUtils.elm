module TestUtils exposing (suite)

import Expect
import Test exposing (..)
import Utils


suite : Test
suite =
    describe "The Utils module"
        [ describe "find"
            [ test "returns Nothing on empty list" <|
                \_ ->
                    Expect.equal Nothing (Utils.find (\_ -> True) [])
            , test "returns `Just 5` from a list of 1 to 10" <|
                \_ ->
                    let
                        list =
                            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    in
                    Expect.equal (Just 5) (Utils.find (\x -> x == 5) list)
            , test "returns Nothing when function always returns False" <|
                \_ ->
                    let
                        list =
                            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    in
                    Expect.equal Nothing (Utils.find (\_ -> False) list)
            ]
        , describe "kernelFilter1"
            [ test "should give empty list when input is empty" <|
                \_ ->
                    Expect.equal [] (Utils.kernelFilter1 (\_ _ _ -> True) [])
            , test "should preserve array when filter always returns true" <|
                \_ ->
                    let
                        list =
                            [ 1, 2, 3, 4, 5 ]
                    in
                    Expect.equal list (Utils.kernelFilter1 (\_ _ _ -> True) list)
            , test "should return empty list when filter always reutrns false" <|
                \_ ->
                    let
                        list =
                            [ 1, 2, 3, 4, 5 ]
                    in
                    Expect.equal [] (Utils.kernelFilter1 (\_ _ _ -> False) list)
            , test "should remove duplicates from sorted list" <|
                \_ ->
                    let
                        list =
                            [ 1, 1, 1, 1, 2, 3, 4, 4, 5 ]

                        filter prev current next =
                            case ( prev, next ) of
                                ( Just p, _ ) ->
                                    current /= p

                                _ ->
                                    True
                    in
                    Expect.equal [ 1, 2, 3, 4, 5 ] (Utils.kernelFilter1 filter list)
            ]
        ]
