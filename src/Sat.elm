module Sat exposing (Problem, Solution, solve)

{-| This library offers a DPLL sat solver and some related utilities. Sat
problems must be converted to CNF form before they can be solved. Elm Sat should
be an alternative to MiniSat.js (via ports) when performance is not critical.

# Types
@docs Problem, Solution

# Solver
@docs solve

-}

import Sat.Model
import Sat.Optimizations
    exposing
        ( assign
        , cmpLiteral
        , pureLiteralAssign
        , unitClauseAssign
        )

{-| Represents a SAT problem in CNF (conjunctive normal form). A problem can be
passed into the `solve` function to search for a solution.

Problem is a type alias for `List (List int)` where integers are names for
boolean variables and the sign indicates if the variable is negated.

    prob : Sat.Problem
    prob = [ [ 1, 2 ], [ -2, 3 ], [ 1 ] ]
-}
type alias Problem =
    Sat.Model.Problem


{-| Represents the boolean assignment of all literals that were present in the
problem which results in SAT Problem simplifying to True. Positive integers
indicate the value is True while negative values indicate False.

    solution : Sat.Solution
    solution = [ 1, -2, -3 ]
-}
type alias Solution =
    Sat.Model.Solution


{-| Find a solution to the provided SAT problem if one exists. If the problem
is UNSAT then `Nothing` will be returned. The solution will always contain
an assignment for every literal in the problem.

    solve [ [ 1, 2 ], [ -2, 3 ], [ 1 ] ] -- [ 1, -2, 3 ]
    solve [ [ -1 ], [ 1 ] ] -- Nothing
-}
solve : Problem -> Maybe Solution
solve problem =
    solveRecurse ( problem, [] ) |> Maybe.map (\s -> List.sortWith cmpLiteral s)


solveRecurse : ( Problem, Solution ) -> Maybe Solution
solveRecurse ( problem, solution ) =
    let
        ( p, s ) =
            ( problem, solution ) |> unitClauseAssign |> pureLiteralAssign
    in
    case p of
        [] ->
            Just s

        [] :: _ ->
            Nothing

        (literal :: _) :: remainingClauses ->
            if List.member [] remainingClauses then
                Nothing

            else
                case solveRecurse (assign literal ( p, s )) of
                    Just assignment ->
                        Just assignment

                    Nothing ->
                        case solveRecurse (assign (negate literal) ( p, s )) of
                            Just assignment ->
                                Just assignment

                            Nothing ->
                                Nothing
