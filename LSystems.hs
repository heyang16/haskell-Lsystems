module LSystems where

import IC.Graphics
import Data.Fixed ( mod' )

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle s = a
  where (a, b, c) = s

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom s = b
  where (a, b, c) = s


-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules s = c
  where (a, b, c) = s

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar c [] = error "No matching character in Rules list"
lookupChar c (r:rs) 
  | c == char = string
  | otherwise = lookupChar c rs
  where 
    (char, string) = r

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne "" r = ""
expandOne [c] r = lookupChar c r
expandOne (c:cs) r = lookupChar c r ++ expandOne cs r

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand s 0 r = s
expand s 1 r = expandOne s r
expand s n r = expand s0 (n - 1) r
  where s0 = expandOne s r

-- Move a turtle
-- 'F': moves forward by 1 unit in the direction currently facing
-- 'R': rotates a degrees clockwise
-- 'L': rotates a degrees anticlockwise
-- after rotation, final angle lies between -180 and 180
move :: Command -> Angle -> TurtleState -> TurtleState
move 'F' a ((x, y), t) = ((x + cos (pi * t / 180), y + sin (pi * t / 180)), t)
move 'R' a (p, t) = (p, if r == -180 then 180 else r)
  where
    r = mod' (t - a + 180) 360 - 180
move 'L' a (p, t) = (p, if l == -180 then 180 else l)
  where
    l = mod' (t + a + 180) 360 - 180
--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--

trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 "" a clr = []
trace1 [c] a clr = undefined
trace1 (c:cs) a clr
  | c == '[' = (, ,clr): trace1 cs a clr
  | c == ']' = undefined : trace1 cs a clr
  | otherwise = undefined : trace1 cs a clr
  where
    

trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2
  |

----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush, canopy, galaxy :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

canopy
  = (30.0,
     "M",
     [('M', "M[+MM][-MM]M[-M][+M]M"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

galaxy
  = (36.0,
     "[M]++[M]++[M]++[M]++[M]",
     [('M', "+M--M---M"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
