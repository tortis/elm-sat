module TestSatOptimizations exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Sat.Optimizations exposing (assign, pureLiteralAssign, unitClauseAssign)
import Test exposing (..)


suite : Test
suite =
    describe "The Sat.Optimizations module"
        [ describe "assign"
            [ test "should reduce a unit clause to an empty problem" <|
                \_ ->
                    let
                        problem =
                            [ [ 1 ] ]
                    in
                    Expect.equal [] (assign 1 problem)
            , test "should return empty clause given conflicting unit clauses" <|
                \_ ->
                    let
                        problem =
                            [ [ 1 ], [ -1 ] ]
                    in
                    Expect.equal [ [] ] (assign 1 problem)
            , test "should remove clause containing the literal" <|
                \_ ->
                    let
                        problem =
                            [ [ 1, 2 ], [ 3, 4 ] ]
                    in
                    Expect.equal [ [ 3, 4 ] ] (assign 1 problem)
            , test "should remove negated literal from clause with other literals" <|
                \_ ->
                    let
                        problem =
                            [ [ 1, 2 ], [ -1, 2 ] ]
                    in
                    Expect.equal [ [ 2 ] ] (assign 1 problem)
            ]
        , describe "pureLiteralAssign"
            [ test "should preserve problem if no pure literals are present" <|
                \_ ->
                    let
                        problem =
                            [ [ 1 ], [ 1, 2, 3 ], [ -1, -2 ], [ -3 ] ]
                    in
                    Expect.equal ( problem, [] )
                        (pureLiteralAssign ( problem, [] ))
            , test "should assign single pure literal" <|
                \_ ->
                    let
                        problem =
                            [ [ 1 ], [ 1, 2, 3 ], [ -1, -2 ], [ 2 ] ]

                        result =
                            [ [ 1 ], [ -1, -2 ], [ 2 ] ]
                    in
                    Expect.equal ( result, [ 3 ] ) (pureLiteralAssign ( problem, [] ))
            , test "should assign multiple pure literals" <|
                \_ ->
                    let
                        problem =
                            [ [ 1 ], [ 1, 2, 3 ], [ 1, -2 ], [ 2 ] ]

                        result =
                            [ [ 2 ] ]
                    in
                    Expect.equal ( result, [ 1, 3 ] ) (pureLiteralAssign ( problem, [] ))
            , test "should fully reduce problem with all pure literals" <|
                \_ ->
                    let
                        problem =
                            [ [ 1 ], [ 1, -2, 3 ], [ 1, -2 ], [ -2 ], [ -5, 6, 7 ], [ -5, 4 ] ]

                        result =
                            []
                    in
                    Expect.equal ( result, [ 1, -2, 3, 4, -5, 6, 7 ] ) (pureLiteralAssign ( problem, [] ))
            ]
        ]
