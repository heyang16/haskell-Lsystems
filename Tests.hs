module Tests where

import IC.TestSuite
import IC.Graphics
import LSystems

import Data.List (sort)

angleTestCases
  = [ cross       ==> 90
    , (1, "", []) ==> 1
    , triangle    ==> 90
    , arrowHead   ==> 60
    ]

axiomTestCases
  = [ (0, "+", []) ==> "+"
    , cross        ==> "M-M-M-M"
    , triangle     ==> "-M"
    , arrowHead    ==> "N"
    ]

rulesTestCases
  = [ cross ==> [ ('M', "M-M+M+MM-M-M+M")
                , ('+', "+")
                , ('-', "-")
                ]
    , (0, "", [ ('M', "N") ])
        ==> [ ('M', "N") ]
    ]

{- Note: these test cases use angle/axiom/rules, and will fail the test
 - suite with Argument exceptions until those functions are correctly
 - implemented.
 -}
lookupCharTestCases
  = [ ('X', [ ('X', "Yes")
            , ('Y', "No")])  ==> "Yes"
    , ('X', [ ('Y', "No")
            , ('X', "Yes")]) ==> "Yes"
    , ('M',  (rules peanoGosper))
      ==> "M+N++N-M--MM-N+"
    , ('+', (rules triangle))
      ==> "+"
    ]

expandOneTestCases
  = [ (axiom triangle, rules triangle)
        ==> "-M+M-M-M+M"
    , ("A", [('A', "B")]) ==> "B"
    ]

expandTestCases
  = [ (axiom arrowHead, 2, rules arrowHead)
        ==> "N+M+N-M-N-M-N+M+N"

    , (axiom dragon, 0, rules dragon)
        ==> "MX"

    , (axiom dragon, 1, rules dragon)
        ==> "A+MX--MY+"

    , (axiom dragon, 5, rules dragon)
        ==> concat [ "A+A+A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--+"
                   , "--A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY---+-"
                   , "-A-A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--++"
                   , "+A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY----+"
                   ]

    , (axiom tree, 2, rules tree)
        ==> "NN[-N[-M][+M][NM]][+N[-M][+M][NM]][NNN[-M][+M][NM]]"
        
    , (axiom bush, 3, rules bush)
        ==> concat [ "MMMM-[[MM-[[M-[[X]+X]+M[+MX]-X]+M-[[X]+X]+M[+MX]-X]+MM["
                   , "+MMM-[[X]+X]+M[+MX]-X]-M-[[X]+X]+M[+MX]-X]+MM-[[M-[[X]"
                   , "+X]+M[+MX]-X]+M-[[X]+X]+M[+MX]-X]+MM[+MMM-[[X]+X]+M[+M"
                   , "X]-X]-M-[[X]+X]+M[+MX]-X]+MMMM[+MMMMMM-[[M-[[X]+X]+M[+"
                   , "MX]-X]+M-[[X]+X]+M[+MX]-X]+MM[+MMM-[[X]+X]+M[+MX]-X]-M"
                   , "-[[X]+X]+M[+MX]-X]-MM-[[M-[[X]+X]+M[+MX]-X]+M-[[X]+X]+"
                   , "M[+MX]-X]+MM[+MMM-[[X]+X]+M[+MX]-X]-M-[[X]+X]+M[+MX]-X"
                   ]
    ]

moveTestCases
  = [ ('L', 90, ((100, 100), 90)) ==> ((100.0,100.0),180.0)
    , ('F', 60, ((50, 50), 60)) ==> ((50.5,50.866024),60.0)
    , ('F', 45, ((-25, 180), 180)) ==> ((-26.0,180.0),180.0)
    , ('R', 90, ((200, 200), 180)) ==> ((200.0, 200.0),90.0)
    , ('R', 45, ((-20, 50), 0)) ==> ((-20.0, 50.0),-45.0)
    , ('R', 90, ((100, 100), 90)) ==> ((100.0,100.0),0.0)
    , ('R', 30, ((100, 100), 90)) ==> ((100.0,100.0),60.0)
    ]

traceTestCases
  = [ ((expandOne (expand (axiom triangle) 1 (rules triangle)) commandMap),
      (angle triangle), blue)
      ==> sort [ ((0.0,0.0),(1.0,0.0),(0.0,0.0,1.0))
               , ((1.0,0.0),(0.99999994,1.0),(0.0,0.0,1.0))
               , ((0.99999994,1.0),(2.0,1.0),(0.0,0.0,1.0))
               , ((2.0,1.0),(2.0,0.0),(0.0,0.0,1.0))
               , ((2.0,0.0),(3.0,0.0),(0.0,0.0,1.0))
               ],
       ((expandOne (expand (axiom tree) 1 (rules tree)) commandMap),
       (angle tree), red)
      ==> sort [ ((0.0,0.0),(-4.371139e-8,1.0),(1.0,0.0,0.0))
                ,((-4.371139e-8,1.0),(0.7071067,1.7071068),(1.0,0.0,0.0))
                ,((-4.371139e-8,1.0),(-0.7071068,1.7071068),(1.0,0.0,0.0))
                ,((-4.371139e-8,1.0),(-8.742278e-8,2.0),(1.0,0.0,0.0))
                ,((-8.742278e-8,2.0),(-1.3113416e-7,3.0),(1.0,0.0,0.0))
               ],
       ((expandOne (expand (axiom galaxy) 1 (rules galaxy)) commandMap), 
        (angle galaxy), blue)
      ==> sort [ ((0.0,0.0),(-0.5877852,0.809017),(0.0,0.0,1.0))
                ,((-0.5877852,0.809017),(0.0,1.6180341),(0.0,0.0,1.0))
                ,((0.0,1.6180341),(0.5877852,0.80901706),(0.0,0.0,1.0))
                ,((0.0,0.0),(-0.9510564,-0.3090172),(0.0,0.0,1.0))
                ,((-0.9510564,-0.3090172),(-1.5388416,0.4999998),(0.0,0.0,1.0))
                ,((-1.5388416,0.4999998),(-0.58778507,0.8090168),(0.0,0.0,1.0))
                ,((0.0,0.0),(1.1924881e-8,-1.0),(0.0,0.0,1.0))
                ,((1.1924881e-8,-1.0),(-0.9510564,-1.3090172),(0.0,0.0,1.0))
                ,((-0.9510564,-1.3090172),(-0.9510565,-0.30901718),(0.0,0.0,1.0))
                ,((0.0,0.0),(0.95105654,-0.30901694),(0.0,0.0,1.0))
                ,((0.95105654,-0.30901694),(0.95105654,-1.309017),(0.0,0.0,1.0))
                ,((0.95105654,-1.309017),(5.9604645e-8,-0.9999999),(0.0,0.0,1.0))
                ,((0.0,0.0),(0.5877852,0.80901706),(0.0,0.0,1.0))
                ,((0.5877852,0.80901706),(1.5388417,0.5000001),(0.0,0.0,1.0))
                ,((1.5388417,0.5000001),(0.9510563,-0.3090167),(0.0,0.0,1.0))
               ],
        ((expandOne (expand (axiom canopy) 1 (rules canopy)) commandMap), 
        (angle canopy), blue)
      ==> sort [ ((0.0,0.0),(-4.371139e-8,1.0),(0.0,0.0,1.0))
                ,((-4.371139e-8,1.0),(-0.5000001,1.8660254),(0.0,0.0,1.0))
                ,((-0.5000001,1.8660254),(-1.0000002,2.732051),(0.0,0.0,1.0))
                ,((-4.371139e-8,1.0),(0.49999994,1.8660254),(0.0,0.0,1.0))
                ,((0.49999994,1.8660254),(0.9999999,2.732051),(0.0,0.0,1.0))
                ,((-4.371139e-8,1.0),(-8.742278e-8,2.0),(0.0,0.0,1.0))
                ,((-8.742278e-8,2.0),(0.49999988,2.8660254),(0.0,0.0,1.0))
                ,((-8.742278e-8,2.0),(-0.5000001,2.8660254),(0.0,0.0,1.0))
                ,((-8.742278e-8,2.0),(-1.3113416e-7,3.0),(0.0,0.0,1.0))
               ]
    ]


allTestCases
  = [ TestCase "angle"      (angle . unId)
                            (map mkId angleTestCases)
    , TestCase "axiom"      (axiom . unId)
                            (map mkId axiomTestCases)
    , TestCase "rules"      (rules . unId)
                            (map mkId rulesTestCases)
    , TestCase "lookupChar" (uncurry lookupChar)
                            lookupCharTestCases
    , TestCase "expandOne"  (uncurry expandOne)
                            expandOneTestCases
    , TestCase "expand"     (uncurry3 expand)
                            expandTestCases
    , TestCase "move"       (uncurry3 move)
                            moveTestCases
    , TestCase "trace1"     (sort . (uncurry3 trace1))
                            traceTestCases
    , TestCase "trace2"     (sort . (uncurry3 trace2))
                            traceTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
