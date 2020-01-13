module Sat exposing (solve)

import Sat.Model exposing (Problem, Solution)
import Sat.Optimizations exposing (assign, pureLiteralAssign, unitClauseAssign, cmpLiteral)


solve : Problem -> Maybe Solution
solve problem =
    solveRecurse (problem, []) |> Maybe.map (\s -> List.sortWith cmpLiteral s)

solveRecurse : (Problem, Solution) -> Maybe Solution
solveRecurse (problem, solution) =
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
                case solveRecurse (assign literal (p, s)) of
                    Just assignment ->
                        Just assignment

                    Nothing ->
                        case solveRecurse (assign (negate literal) (p, s)) of
                            Just assignment ->
                                Just assignment

                            Nothing ->
                                Nothing

