module MySat exposing (..)

import Debug


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
            unitClauseAssign ( problem, [] )
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



pureLiteralAssign : Literal -> Problem -> Problem
pureLiteralAssign literal problem =
    let
        allLiterals =
            List.foldl (++) [] problem

        fn : List Literal -> List Literal -> List Literal -> List Literal
        fn literals stack pures =
            case (literals, stack) of
                ([], []) ->
                    pures

                (l :: restLiterals, []) ->
                    fn restLiterals [] (l :: pures)

                ([], l :: restStack) ->
                    fn [] restStack (l :: pures)

                (l :: restLiterals, s :: restStack) ->
                    
    in
    if isPure then
        List.filter (\clause -> not (List.member literal clause)) problem

    else
        problem


unitClauseAssign : ( Problem, Solution ) -> ( Problem, Solution )
unitClauseAssign ( problem, solution ) =
    case find (\clause -> List.length clause == 1) problem of
        Just (literal :: []) ->
            unitClauseAssign ( assign literal problem, literal :: solution )

        _ ->
            ( problem, solution )


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
