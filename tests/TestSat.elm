module TestSat exposing (suite)

import Expect
import Sat exposing (solve)
import Sat.Utils exposing (fromDimacs)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "The Sat module"
        [ describe "solve"
            [ test "should return empty solution for empty problem" <|
                \_ ->
                    let
                        problem =
                            []
                    in
                    Expect.equal (Just []) (solve problem)
            , test "should correctly solve single unit clause" <|
                \_ ->
                    let
                        problem =
                            [ [ -1 ] ]
                    in
                    Expect.equal (Just [ -1 ]) (solve problem)
            , test "should not find solution for simple contradiction" <|
                \_ ->
                    let
                        problem =
                            [ [ 1 ], [ -1 ] ]
                    in
                    Expect.equal Nothing (solve problem)
            , test "Should find solution for kaboom example" <|
                \_ ->
                    let
                        problem =
                            [ [ 1, 2 ], [ 1, 3 ], [ 2, 3 ], [ -1, -2, -3 ], [ 2, 3 ], [ -2, -3 ] ]

                        result =
                            solve problem
                    in
                    case result of
                        Nothing ->
                            Expect.fail ""
                        Just assignment ->
                            Expect.equalSets (Set.fromList [1, 2, -3]) (Set.fromList assignment)
            , test "factoring 44" <|
                \_ ->
                    let
                        correctAssignments = [-1, 2, -3, -4, -5, -6, -7, 8, 9, -10, 11, -12]
                        result = solve (fromDimacs factoringProblem) |> Maybe.withDefault []
                    in
                    List.map (\l -> List.member l result) correctAssignments
                        |> List.foldl (&&) True
                        |> Expect.equal True
            ]
        ]

factoringProblem : String
factoringProblem = """
p cnf 118 548
c Factors encoded in variables 1-6 and 7-12
c Target number: 44
2 3 4 5 6 0
8 9 10 11 12 0
-13 0
14 0
-51 16 21 0
-51 -16 -21 0
51 16 -21 0
51 -16 21 0
-16 -21 52 0
16 21 -52 0
16 -52 0
21 -52 0
-53 17 22 52 0
-53 -17 -22 52 0
-53 -17 22 -52 0
-53 17 -22 -52 0
53 -17 -22 -52 0
53 17 22 -52 0
53 17 -22 52 0
53 -17 22 52 0
-17 -22 54 0
-17 -52 54 0
-22 -52 54 0
17 22 -54 0
17 52 -54 0
22 52 -54 0
-55 18 23 54 0
-55 -18 -23 54 0
-55 -18 23 -54 0
-55 18 -23 -54 0
55 -18 -23 -54 0
55 18 23 -54 0
55 18 -23 54 0
55 -18 23 54 0
-18 -23 56 0
-18 -54 56 0
-23 -54 56 0
18 23 -56 0
18 54 -56 0
23 54 -56 0
-57 19 24 56 0
-57 -19 -24 56 0
-57 -19 24 -56 0
-57 19 -24 -56 0
57 -19 -24 -56 0
57 19 24 -56 0
57 19 -24 56 0
57 -19 24 56 0
-19 -24 58 0
-19 -56 58 0
-24 -56 58 0
19 24 -58 0
19 56 -58 0
24 56 -58 0
-59 20 25 58 0
-59 -20 -25 58 0
-59 -20 25 -58 0
-59 20 -25 -58 0
59 -20 -25 -58 0
59 20 25 -58 0
59 20 -25 58 0
59 -20 25 58 0
-20 -25 60 0
-20 -58 60 0
-25 -58 60 0
20 25 -60 0
20 58 -60 0
25 58 -60 0
-61 26 60 0
-61 -26 -60 0
61 26 -60 0
61 -26 60 0
-26 -60 62 0
26 60 -62 0
26 -62 0
60 -62 0
-63 28 33 0
-63 -28 -33 0
63 28 -33 0
63 -28 33 0
-28 -33 64 0
28 33 -64 0
28 -64 0
33 -64 0
-65 29 34 64 0
-65 -29 -34 64 0
-65 -29 34 -64 0
-65 29 -34 -64 0
65 -29 -34 -64 0
65 29 34 -64 0
65 29 -34 64 0
65 -29 34 64 0
-29 -34 66 0
-29 -64 66 0
-34 -64 66 0
29 34 -66 0
29 64 -66 0
34 64 -66 0
-67 30 35 66 0
-67 -30 -35 66 0
-67 -30 35 -66 0
-67 30 -35 -66 0
67 -30 -35 -66 0
67 30 35 -66 0
67 30 -35 66 0
67 -30 35 66 0
-30 -35 68 0
-30 -66 68 0
-35 -66 68 0
30 35 -68 0
30 66 -68 0
35 66 -68 0
-69 31 36 68 0
-69 -31 -36 68 0
-69 -31 36 -68 0
-69 31 -36 -68 0
69 -31 -36 -68 0
69 31 36 -68 0
69 31 -36 68 0
69 -31 36 68 0
-31 -36 70 0
-31 -68 70 0
-36 -68 70 0
31 36 -70 0
31 68 -70 0
36 68 -70 0
-71 32 37 70 0
-71 -32 -37 70 0
-71 -32 37 -70 0
-71 32 -37 -70 0
71 -32 -37 -70 0
71 32 37 -70 0
71 32 -37 70 0
71 -32 37 70 0
-32 -37 72 0
-32 -70 72 0
-37 -70 72 0
32 37 -72 0
32 70 -72 0
37 70 -72 0
-73 38 72 0
-73 -38 -72 0
73 38 -72 0
73 -38 72 0
-38 -72 74 0
38 72 -74 0
38 -74 0
72 -74 0
-75 40 45 0
-75 -40 -45 0
75 40 -45 0
75 -40 45 0
-40 -45 76 0
40 45 -76 0
40 -76 0
45 -76 0
-77 41 46 76 0
-77 -41 -46 76 0
-77 -41 46 -76 0
-77 41 -46 -76 0
77 -41 -46 -76 0
77 41 46 -76 0
77 41 -46 76 0
77 -41 46 76 0
-41 -46 78 0
-41 -76 78 0
-46 -76 78 0
41 46 -78 0
41 76 -78 0
46 76 -78 0
-79 42 47 78 0
-79 -42 -47 78 0
-79 -42 47 -78 0
-79 42 -47 -78 0
79 -42 -47 -78 0
79 42 47 -78 0
79 42 -47 78 0
79 -42 47 78 0
-42 -47 80 0
-42 -78 80 0
-47 -78 80 0
42 47 -80 0
42 78 -80 0
47 78 -80 0
-81 43 48 80 0
-81 -43 -48 80 0
-81 -43 48 -80 0
-81 43 -48 -80 0
81 -43 -48 -80 0
81 43 48 -80 0
81 43 -48 80 0
81 -43 48 80 0
-43 -48 82 0
-43 -80 82 0
-48 -80 82 0
43 48 -82 0
43 80 -82 0
48 80 -82 0
-83 44 49 82 0
-83 -44 -49 82 0
-83 -44 49 -82 0
-83 44 -49 -82 0
83 -44 -49 -82 0
83 44 49 -82 0
83 44 -49 82 0
83 -44 49 82 0
-44 -49 84 0
-44 -82 84 0
-49 -82 84 0
44 49 -84 0
44 82 -84 0
49 82 -84 0
-85 50 84 0
-85 -50 -84 0
85 50 -84 0
85 -50 84 0
-50 -84 86 0
50 84 -86 0
50 -86 0
84 -86 0
-87 53 27 0
-87 -53 -27 0
87 53 -27 0
87 -53 27 0
-53 -27 88 0
53 27 -88 0
53 -88 0
27 -88 0
-89 55 63 88 0
-89 -55 -63 88 0
-89 -55 63 -88 0
-89 55 -63 -88 0
89 -55 -63 -88 0
89 55 63 -88 0
89 55 -63 88 0
89 -55 63 88 0
-55 -63 90 0
-55 -88 90 0
-63 -88 90 0
55 63 -90 0
55 88 -90 0
63 88 -90 0
-91 57 65 90 0
-91 -57 -65 90 0
-91 -57 65 -90 0
-91 57 -65 -90 0
91 -57 -65 -90 0
91 57 65 -90 0
91 57 -65 90 0
91 -57 65 90 0
-57 -65 92 0
-57 -90 92 0
-65 -90 92 0
57 65 -92 0
57 90 -92 0
65 90 -92 0
-93 59 67 92 0
-93 -59 -67 92 0
-93 -59 67 -92 0
-93 59 -67 -92 0
93 -59 -67 -92 0
93 59 67 -92 0
93 59 -67 92 0
93 -59 67 92 0
-59 -67 94 0
-59 -92 94 0
-67 -92 94 0
59 67 -94 0
59 92 -94 0
67 92 -94 0
-95 61 69 94 0
-95 -61 -69 94 0
-95 -61 69 -94 0
-95 61 -69 -94 0
95 -61 -69 -94 0
95 61 69 -94 0
95 61 -69 94 0
95 -61 69 94 0
-61 -69 96 0
-61 -94 96 0
-69 -94 96 0
61 69 -96 0
61 94 -96 0
69 94 -96 0
-97 62 71 96 0
-97 -62 -71 96 0
-97 -62 71 -96 0
-97 62 -71 -96 0
97 -62 -71 -96 0
97 62 71 -96 0
97 62 -71 96 0
97 -62 71 96 0
-62 -71 98 0
-62 -96 98 0
-71 -96 98 0
62 71 -98 0
62 96 -98 0
71 96 -98 0
-99 73 98 0
-99 -73 -98 0
99 73 -98 0
99 -73 98 0
-73 -98 100 0
73 98 -100 0
73 -100 0
98 -100 0
-101 74 100 0
-101 -74 -100 0
101 74 -100 0
101 -74 100 0
-74 -100 102 0
74 100 -102 0
74 -102 0
100 -102 0
-103 91 39 0
-103 -91 -39 0
103 91 -39 0
103 -91 39 0
-91 -39 104 0
91 39 -104 0
91 -104 0
39 -104 0
-105 93 75 104 0
-105 -93 -75 104 0
-105 -93 75 -104 0
-105 93 -75 -104 0
105 -93 -75 -104 0
105 93 75 -104 0
105 93 -75 104 0
105 -93 75 104 0
-93 -75 106 0
-93 -104 106 0
-75 -104 106 0
93 75 -106 0
93 104 -106 0
75 104 -106 0
-107 95 77 106 0
-107 -95 -77 106 0
-107 -95 77 -106 0
-107 95 -77 -106 0
107 -95 -77 -106 0
107 95 77 -106 0
107 95 -77 106 0
107 -95 77 106 0
-95 -77 108 0
-95 -106 108 0
-77 -106 108 0
95 77 -108 0
95 106 -108 0
77 106 -108 0
-109 97 79 108 0
-109 -97 -79 108 0
-109 -97 79 -108 0
-109 97 -79 -108 0
109 -97 -79 -108 0
109 97 79 -108 0
109 97 -79 108 0
109 -97 79 108 0
-97 -79 110 0
-97 -108 110 0
-79 -108 110 0
97 79 -110 0
97 108 -110 0
79 108 -110 0
-111 99 81 110 0
-111 -99 -81 110 0
-111 -99 81 -110 0
-111 99 -81 -110 0
111 -99 -81 -110 0
111 99 81 -110 0
111 99 -81 110 0
111 -99 81 110 0
-99 -81 112 0
-99 -110 112 0
-81 -110 112 0
99 81 -112 0
99 110 -112 0
81 110 -112 0
-113 101 83 112 0
-113 -101 -83 112 0
-113 -101 83 -112 0
-113 101 -83 -112 0
113 -101 -83 -112 0
113 101 83 -112 0
113 101 -83 112 0
113 -101 83 112 0
-101 -83 114 0
-101 -112 114 0
-83 -112 114 0
101 83 -114 0
101 112 -114 0
83 112 -114 0
-115 102 85 114 0
-115 -102 -85 114 0
-115 -102 85 -114 0
-115 102 -85 -114 0
115 -102 -85 -114 0
115 102 85 -114 0
115 102 -85 114 0
115 -102 85 114 0
-102 -85 116 0
-102 -114 116 0
-85 -114 116 0
102 85 -116 0
102 114 -116 0
85 114 -116 0
-117 86 116 0
-117 -86 -116 0
117 86 -116 0
117 -86 116 0
-86 -116 118 0
86 116 -118 0
86 -118 0
116 -118 0
15 -1 -7 0
-15 1 0
-15 7 0
16 -2 -7 0
-16 2 0
-16 7 0
17 -3 -7 0
-17 3 0
-17 7 0
18 -4 -7 0
-18 4 0
-18 7 0
19 -5 -7 0
-19 5 0
-19 7 0
20 -6 -7 0
-20 6 0
-20 7 0
21 -1 -8 0
-21 1 0
-21 8 0
22 -2 -8 0
-22 2 0
-22 8 0
23 -3 -8 0
-23 3 0
-23 8 0
24 -4 -8 0
-24 4 0
-24 8 0
25 -5 -8 0
-25 5 0
-25 8 0
26 -6 -8 0
-26 6 0
-26 8 0
27 -1 -9 0
-27 1 0
-27 9 0
28 -2 -9 0
-28 2 0
-28 9 0
29 -3 -9 0
-29 3 0
-29 9 0
30 -4 -9 0
-30 4 0
-30 9 0
31 -5 -9 0
-31 5 0
-31 9 0
32 -6 -9 0
-32 6 0
-32 9 0
33 -1 -10 0
-33 1 0
-33 10 0
34 -2 -10 0
-34 2 0
-34 10 0
35 -3 -10 0
-35 3 0
-35 10 0
36 -4 -10 0
-36 4 0
-36 10 0
37 -5 -10 0
-37 5 0
-37 10 0
38 -6 -10 0
-38 6 0
-38 10 0
39 -1 -11 0
-39 1 0
-39 11 0
40 -2 -11 0
-40 2 0
-40 11 0
41 -3 -11 0
-41 3 0
-41 11 0
42 -4 -11 0
-42 4 0
-42 11 0
43 -5 -11 0
-43 5 0
-43 11 0
44 -6 -11 0
-44 6 0
-44 11 0
45 -1 -12 0
-45 1 0
-45 12 0
46 -2 -12 0
-46 2 0
-46 12 0
47 -3 -12 0
-47 3 0
-47 12 0
48 -4 -12 0
-48 4 0
-48 12 0
49 -5 -12 0
-49 5 0
-49 12 0
50 -6 -12 0
-50 6 0
-50 12 0
15 -13 0
-15 13 0
51 -13 0
-51 13 0
87 -14 0
-87 14 0
89 -14 0
-89 14 0
103 -13 0
-103 13 0
105 -14 0
-105 14 0
107 -13 0
-107 13 0
109 -13 0
-109 13 0
111 -13 0
-111 13 0
113 -13 0
-113 13 0
115 -13 0
-115 13 0
117 -13 0
-117 13 0
118 -13 0
-118 13 0
"""
