module TestSatOptimizations exposing (suite)

import Expect
import Sat.Optimizations exposing (assign, pureLiteralAssign, unitClauseAssign)
import Set
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
                    Expect.equal ([], [1]) (assign 1 (problem, []))
            , test "should return empty clause given conflicting unit clauses" <|
                \_ ->
                    let
                        problem =
                            [ [ 1 ], [ -1 ] ]
                    in
                    Expect.equal ([ [] ], [1]) (assign 1 (problem, []))
            , test "should remove clause containing the literal" <|
                \_ ->
                    let
                        problem =
                            [ [ 1, 2 ], [ 3, 4 ] ]
                    in
                    Expect.equal ([ [ 3, 4 ] ], [1, 2]) (assign 1 (problem, []))
            , test "should remove negated literal from clause with other literals" <|
                \_ ->
                    let
                        problem =
                            [ [ 1, 2 ], [ -1, 2 ] ]
                    in
                    Expect.equal ([ [ 2 ] ], [1]) (assign 1 (problem, []))
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

                        expected =
                            ([], [ 1, -2, 3, 4, -5, 6, 7 ] |> Set.fromList)

                        (resultProblem, resultAssignment) = pureLiteralAssign (problem, [])

                        result =
                            (resultProblem, Set.fromList resultAssignment)
                    in
                    Expect.equal expected result
            ]
        , describe "unitClauseAssign"
            [ test "should preserve empty problem" <|
                \_ ->
                    let
                        problem =
                            []
                    in
                    Expect.equal ( [], [] ) (unitClauseAssign ( problem, [] ))
            , test "should preserve problem with no unit clauses" <|
                \_ ->
                    let
                        problem =
                            [ [ 1, 2 ], [ 3, 4, 5 ], [ 1, 3, 2 ] ]
                    in
                    Expect.equal ( problem, [] ) (unitClauseAssign ( problem, [] ))
            , test "should remove single unit clause" <|
                \_ ->
                    let
                        problem =
                            [ [ 9 ], [ 1, 2 ], [ 3, 4, 5 ], [ 1, 3, 2 ] ]

                        result =
                            [ [ 1, 2 ], [ 3, 4, 5 ], [ 1, 3, 2 ] ]
                    in
                    Expect.equal ( result, [ 9 ] ) (unitClauseAssign ( problem, [] ))
            , test "should remove multiple unit clauses" <|
                \_ ->
                    let
                        problem =
                            [ [ 9 ], [ 1, 2 ], [ 3, 4, 5 ], [ 1, 3, 2 ], [ 8 ] ]

                        expect =
                            [ [ 1, 2 ], [ 3, 4, 5 ], [ 1, 3, 2 ] ]

                        result =
                            unitClauseAssign ( problem, [] )
                    in
                    Expect.all
                        [ \r -> Expect.equal expect (Tuple.first r)
                        , \r -> Expect.equalSets (Set.fromList [ 9, 8 ]) (Set.fromList (Tuple.second r))
                        ]
                        result
            , test "should remove unit clause and clauses containing the literal" <|
                \_ ->
                    let
                        problem =
                            [ [ 3 ], [ 1, 2 ], [ 3, 4, 5 ], [ 1, 3, 2 ] ]

                        result =
                            [ [ 1, 2 ] ]
                    in
                    Expect.equal ( result, [ 3, 4, 5 ] ) (unitClauseAssign ( problem, [] ))
            ]
        ]
