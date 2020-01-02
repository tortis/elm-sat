module Sat exposing (..)


type alias Literal =
    Int


type alias Clause =
    List Literal


type alias Problem =
    List Clause


type alias Assignment =
    List Literal


example : Problem
example =
    [ [ 1, 2 ], [ -2, 3 ] ]


kaboom : Problem
kaboom =
    [ [ 1, 2 ], [ 1, 3 ], [ 2, 3 ], [ -1, -2, -3 ], [ 2, 3 ], [ -2, -3 ] ]


propagate : Literal -> Problem -> Problem
propagate literal problem =
    let
        filteredClauses =
            List.filter (\clause -> not (List.member literal clause)) problem
    in
    List.map (\clause -> List.filter (\l -> l /= negate literal) clause) filteredClauses


solve : Problem -> Maybe Assignment
solve problem =
    case problem of
        [] ->
            Just []

        [] :: _ ->
            Nothing

        (literal :: clauseRest) :: problemRest ->
            case solve (propagate literal problemRest) of
                Just assignment ->
                    Just (literal :: assignment)

                Nothing ->
                    case solve (propagate (negate literal) (clauseRest :: problemRest)) of
                        Just assignment ->
                            Just (negate literal :: assignment)

                        Nothing ->
                            Nothing
