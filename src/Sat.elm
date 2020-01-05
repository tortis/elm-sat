module Sat exposing (solve)

import Sat.Optimizations exposing (assign, pureLiteralAssign, unitClauseAssign)
import Sat.Model exposing (Problem, Solution)


solve : Problem -> Maybe Solution
solve problem =
    let
        ( p, s ) =
            ( problem, [] ) |> pureLiteralAssign |> unitClauseAssign
    in
    case p of
        [] ->
            Just s

        [] :: _ ->
            Nothing

        (literal :: remainingLiterals) :: remainingClauses ->
            if List.member [] remainingClauses then
                Nothing

            else
                case solve (assign literal remainingClauses) of
                    Just solution ->
                        Just (literal :: solution)

                    Nothing ->
                        case solve (assign (negate literal) (remainingLiterals :: remainingClauses)) of
                            Just solution ->
                                Just (negate literal :: solution)

                            Nothing ->
                                Nothing


