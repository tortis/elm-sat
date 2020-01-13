module Sat.Optimizations exposing (assign, cmpLiteral, pureLiteralAssign, unitClauseAssign)

import Sat.Model exposing (Literal, Problem, Solution)
import Sat.Utils exposing (find, kernelFilter1)
import Set


assign : Literal -> ( Problem, Solution ) -> ( Problem, Solution )
assign literal ( problem, solution ) =
    let
        ( withLiteral, withoutLiteral ) =
            List.partition (\clause -> List.member literal clause) problem

        simplifiedProblem =
            List.map (\clause -> List.filter (\l -> l /= negate literal) clause) withoutLiteral

        eliminatedLiterals =
            List.foldl (++) [] withLiteral |> Set.fromList

        filteredEliminatedLiterals =
            Set.filter (\l -> l > 0 || not (Set.member (negate l) eliminatedLiterals)) eliminatedLiterals

        remainingLiterals =
            List.foldl (++) [] simplifiedProblem |> Set.fromList

        assignments =
            Set.filter (\l -> not (Set.member l remainingLiterals || Set.member (negate l) remainingLiterals)) filteredEliminatedLiterals |> Set.toList
    in
    ( simplifiedProblem, assignments ++ solution )


pureLiteralAssign : ( Problem, Solution ) -> ( Problem, Solution )
pureLiteralAssign ( problem, solution ) =
    let
        pureLiterals =
            List.foldl (++) [] problem
                |> List.sortWith cmpLiteral
                |> kernelFilter1 uniqueFilter
                |> kernelFilter1 pureFilter

        uniqueFilter : Maybe Literal -> Literal -> Maybe Literal -> Bool
        uniqueFilter prev current next =
            case ( prev, next ) of
                ( Just prevLiteral, _ ) ->
                    current /= prevLiteral

                _ ->
                    True

        pureFilter : Maybe Literal -> Literal -> Maybe Literal -> Bool
        pureFilter prev current next =
            case ( prev, next ) of
                ( Just prevLiteral, Just nextLiteral ) ->
                    current + prevLiteral /= 0 && current + nextLiteral /= 0

                ( Just prevLiteral, Nothing ) ->
                    current + prevLiteral /= 0

                ( Nothing, Just nextLiteral ) ->
                    current + nextLiteral /= 0

                ( Nothing, Nothing ) ->
                    True
    in
    List.foldl assign ( problem, solution ) pureLiterals


cmpLiteral : Literal -> Literal -> Order
cmpLiteral lhs rhs =
    let
        absOrd =
            compare (abs lhs) (abs rhs)
    in
    case absOrd of
        EQ ->
            compare lhs rhs

        _ ->
            absOrd


unitClauseAssign : ( Problem, Solution ) -> ( Problem, Solution )
unitClauseAssign ( problem, solution ) =
    case find (\clause -> List.length clause == 1) problem of
        Just (literal :: []) ->
            unitClauseAssign (assign literal ( problem, solution ))

        _ ->
            ( problem, solution )

