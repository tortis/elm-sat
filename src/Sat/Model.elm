module Sat.Model exposing (Clause, Literal, Problem, Solution)

type alias Problem =
    List Clause


type alias Clause =
    List Literal


type alias Literal =
    Int


type alias Solution =
    List Literal
