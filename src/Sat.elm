module Sat exposing (..) -- (Clause, Literal, Problem, Solution, solve)

import Utils


type alias Problem =
    List Clause


type alias Clause =
    List Literal


type alias Literal =
    Int


type alias Solution =
    List Literal


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


assign : Literal -> Problem -> Problem
assign literal problem =
    let
        filteredClauses =
            List.filter (\clause -> not (List.member literal clause)) problem
    in
    List.map (\clause -> List.filter (\l -> l /= negate literal) clause) filteredClauses


pureLiteralAssign : ( Problem, Solution ) -> ( Problem, Solution )
pureLiteralAssign ( problem, solution ) =
    let
        pureLiterals =
            List.foldl (++) [] problem
                |> List.sortWith cmpLiteral
                |> Utils.kernelFilter1 uniqueFilter
                |> Utils.kernelFilter1 pureFilter

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
    ( List.foldl assign problem pureLiterals, pureLiterals ++ solution )


unitClauseAssign : ( Problem, Solution ) -> ( Problem, Solution )
unitClauseAssign ( problem, solution ) =
    case Utils.find (\clause -> List.length clause == 1) problem of
        Just (literal :: []) ->
            unitClauseAssign ( assign literal problem, literal :: solution )

        _ ->
            ( problem, solution )
