module Sat.Model exposing (Clause, Literal, Problem, Solution, verify, fromDimacs)

import Utils

type alias Problem =
    List Clause


type alias Clause =
    List Literal


type alias Literal =
    Int


type alias Solution =
    List Literal

verify : Problem -> Solution -> Bool
verify problem solution =
    let
        value : Literal -> Bool
        value literal =
            let
                assignment = Utils.find (\l -> abs l == abs literal) solution
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
                    if List.member word ["p", "c", ""] then
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
