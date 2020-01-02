module MySat exposing (..)


type alias Problem =
    List Clause


type alias Clause =
    List Literal


type alias Literal = Int


type alias Solution =
    List Literal


solve : Problem -> Maybe Solution
solve problem =
    case problem of
        [] ->
            Just []

        [] :: _ ->
            Nothing

        (literal :: remainingLiterals) :: remainingClauses ->
           Just []

splitAssign : Literal
splitAssign = 5

fullPureLiteralAssign : Problem -> (Problem, Solution)
fullPureLiteralAssign problem =
    ([], [])


pureLiteralAssign : Literal -> Problem -> Problem
pureLiteralAssign literal problem =
    let
        allLiterals = List.foldl (++) [] problem
        isPure = not (List.member (negate literal) allLiterals)
    in
        if isPure then
            List.filter (\clause -> not (List.member literal clause)) problem
        else
            problem


simplify : Literal -> Problem -> Problem
simplify literal problem =
    let
        filteredClauses =
            List.filter (\clause -> not (List.member literal clause)) problem
    in
    List.map (\clause -> List.filter (\l -> l /= negate literal) clause) filteredClauses


unitClauseAssign : Problem -> (Problem, Solution)
unitClauseAssign problem =
    let
        unitLiterals : List Literal
        unitLiterals =
            List.map unitClauseToLiteral problem |> List.filterMap identity

        fer literal (p, solution) =
            (simplify literal p, literal :: solution)
    in
        List.foldl fer (problem, []) unitLiterals


unitClauseToLiteral : Clause -> Maybe Literal
unitClauseToLiteral clause =
    case clause of
        literal :: [] ->
            Just literal

        _ ->
            Nothing
