module TestSatModel exposing (suite)

import Expect
import Sat.Model exposing (fromDimacs, verify)
import Test exposing (..)


suite : Test
suite =
    describe "The Sat.Model module"
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
        ]
