module TestSatUtils exposing (suite)

import Expect
import Test exposing (..)
import Sat.Utils exposing (fromDimacs, verify, find, kernelFilter1)


suite : Test
suite =
    describe "The Sat.Utils module"
        [ describe "fromDimacs"
            [ test "should correctly parse DIMACS string" <|
                    \_ ->
                        let
                            dimacs =
                                """
                    p cnf 3 6
                    c test comment
                    1 2 0
                    1 3 0
                    2 3 0
                    -1 -2 -3 0
                    2 3 0
                    -2 -3 0
                    """

                            expect =
                                [ [ 1, 2 ]
                                , [ 1, 3 ]
                                , [ 2, 3 ]
                                , [ -1, -2, -3 ]
                                , [ 2, 3 ]
                                , [ -2, -3 ]
                                ]
                        in
                        Expect.equal expect (fromDimacs dimacs)
            ]
        , describe "verify"
            [ test "single unit clause should return true" <|
                    \_ ->
                        let
                            problem =
                                [ [ 1 ] ]

                            solution =
                                [ 1 ]
                        in
                        Expect.equal True (verify problem solution)
            , test "single unit clause should return false" <|
                    \_ ->
                        let
                            problem =
                                [ [ 1 ] ]

                            solution =
                                [ -1 ]
                        in
                        Expect.equal False (verify problem solution)
            , test "kaboom example should return true" <|
                \_ ->
                    let
                        problem =
                            [ [ 1, 2 ], [ 1, 3 ], [ 2, 3 ], [ -1, -2, -3 ], [ 2, 3 ], [ -2, -3 ] ]

                        solution =
                            [ 1, -2, 3 ]
                    in
                    Expect.equal True (verify problem solution)
            , test "kaboom example should return false" <|
                \_ ->
                    let
                        problem =
                            [ [ 1, 2 ], [ 1, 3 ], [ 2, 3 ], [ -1, -2, -3 ], [ 2, 3 ], [ -2, -3 ] ]

                        solution =
                            [ -1, 2, 3 ]
                    in
                    Expect.equal False (verify problem solution)
            ]
        , describe "find"
            [ test "returns Nothing on empty list" <|
                \_ ->
                    Expect.equal Nothing (find (\_ -> True) [])
            , test "returns `Just 5` from a list of 1 to 10" <|
                \_ ->
                    let
                        list =
                            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    in
                    Expect.equal (Just 5) (find (\x -> x == 5) list)
            , test "returns Nothing when function always returns False" <|
                \_ ->
                    let
                        list =
                            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    in
                    Expect.equal Nothing (find (\_ -> False) list)
            ]
        , describe "kernelFilter1"
            [ test "should give empty list when input is empty" <|
                \_ ->
                    Expect.equal [] (kernelFilter1 (\_ _ _ -> True) [])
            , test "should preserve array when filter always returns true" <|
                \_ ->
                    let
                        list =
                            [ 1, 2, 3, 4, 5 ]
                    in
                    Expect.equal list (kernelFilter1 (\_ _ _ -> True) list)
            , test "should return empty list when filter always reutrns false" <|
                \_ ->
                    let
                        list =
                            [ 1, 2, 3, 4, 5 ]
                    in
                    Expect.equal [] (kernelFilter1 (\_ _ _ -> False) list)
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
                    Expect.equal [ 1, 2, 3, 4, 5 ] (kernelFilter1 filter list)
            ]
        ]
