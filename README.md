# Elm Sat [![pipeline status](https://gitlab.com/findley/elm-sat/badges/master/pipeline.svg)](https://gitlab.com/findley/elm-sat/commits/master)
A simple [DPLL](https://en.wikipedia.org/wiki/DPLL_algorithm) sat solver implemented in Elm.

Note: This repository is maintained at https://gitlab.com/findley/elm-sat and mirrored to https://github.com/tortis/elm-sat.

## Usage
The Sat.Model module exposes the `Problem` type and a function `fromDimacs` which will turn a DIMACS formatted string into a Problem.

The Problem type is just an alias for `List (List Int)`. Variables (literals) are represented by integers where a negative integer indicates negation of the literal.

Sat problems must be in [CNF]() form. 

```Elm
import Sat
import Sat.Problem

problem : Sat.Model.Problem
problem = [ [ 1, 2 ], [ 1, 3 ], [ 2, 3 ], [ -1, -2, -3 ], [ 2, 3 ], [ -2, -3 ] ]

solution : Maybe Sat.Model.Solution
solution = Sat.solve problem
-- Just [1,2,-3]
```

## Performance
Elm Sat performance appears to be worse than [MiniSAT compiled to JS](https://jgalenson.github.io/research.js/demos/minisat.html). Adding benchmarks and improving performance are areas for future development.

## License
This code is distributed under the MIT license, see the [LICENSE](LICENSE) file.