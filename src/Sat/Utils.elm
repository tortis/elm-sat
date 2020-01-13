module Sat.Utils exposing (find, fromDimacs, kernelFilter1, verify)

{-| This sub-module offers some utilities related to SAT solving. It also
contains a few general purpose functions primarily for internal use.

# SAT Problems
@docs fromDimacs, verify

# DPLL Helpers
@docs find, kernelFilter1

-}

import Sat.Model exposing (Clause, Literal, Problem, Solution)


{-| Check if a given solution satifies a problem. This useful for sanity checks
and unit testing.

    verify [ [ 1, 2 ], [ -2, 3 ], [ 1 ] ] [ 1, -2, 3 ] -- True
    verify [ [ 1, 2 ], [ -2, 3 ], [ 1 ] ] [ 1, -2, -3 ] -- True
    verify [ [ 1, 2 ], [ -2, 3 ], [ 1 ] ] [ -1, -2, -3 ] -- False
-}
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


{-| Create a `Sat.Problem` from DIMACS formatted string. Only `cnf`
representation is supported. The problem line and comment lines will be
ignored.

    fromDimacs """
    c  simple_v3_c2.cnf
    c
    p cnf 3 2
    1 -3 0
    2 3 -1 0
    """ -- [ [ 1, -3 ], [ 2, 3, -1 ] ]

-}
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


{-| Find the first occurence of a list item that matches the filter.

    find (\v -> v % 2 == 0) [ 1, 2, 3, 4 ] -- Just 2
-}
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


{-| Filter list items based on each items nearest neighbors.

    touchesOne prev _ next =
        let
            isOne neighbor =
                Maybe.map (\v -> v == 1) neighbor |> Maybe.withDefault False

        in
        isOne prev || isOne next

    kernelFilter1 touchesOne [ 1, 2, 3, 4, 1, 5, 6 ] -- [ 2, 4, 5 ]
-}
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
