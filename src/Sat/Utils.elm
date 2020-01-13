module Sat.Utils exposing (find, fromDimacs, kernelFilter1, verify)

import Sat.Model exposing (Clause, Literal, Problem, Solution)


verify : Problem -> Solution -> Bool
verify problem solution =
    let
        value : Literal -> Bool
        value literal =
            let
                assignment =
                    find (\l -> abs l == abs literal) solution
            in
            case assignment of
                Nothing ->
                    if literal < 0 then
                        True

                    else
                        False

                Just assignedLiteral ->
                    literal == assignedLiteral

        evalClause : Clause -> Bool
        evalClause clause =
            clause |> List.map value |> List.foldl (||) False
    in
    problem |> List.map evalClause |> List.foldl (&&) True


fromDimacs : String -> Problem
fromDimacs dimacs =
    let
        isClauseLine : List String -> Bool
        isClauseLine line =
            case List.head line of
                Nothing ->
                    False

                Just word ->
                    if List.member word [ "p", "c", "" ] then
                        False

                    else
                        True

        lineToClause : List String -> Clause
        lineToClause line =
            line |> List.filter (\w -> w /= "0") |> List.filterMap String.toInt
    in
    dimacs
        |> String.lines
        |> List.map String.words
        |> List.filter isClauseLine
        |> List.map lineToClause


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


kernelFilter1 : (Maybe a -> a -> Maybe a -> Bool) -> List a -> List a
kernelFilter1 filter list =
    let
        kernelFilterHelp : List a -> Maybe a -> List a
        kernelFilterHelp innerList prev =
            case innerList of
                [] ->
                    []

                item :: rest ->
                    if filter prev item (List.head rest) then
                        item :: kernelFilterHelp rest (Just item)

                    else
                        kernelFilterHelp rest (Just item)
    in
    kernelFilterHelp list Nothing
