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
-- As facing -180 degrees is the same as facing 180 degrees, we set 180 degrees to be the standard
move :: Command -> Angle -> TurtleState -> TurtleState
move 'F' a ((x, y), t) = ((x + cos ((pi / 180) * t), y + sin ((pi / 180) * t)), t)
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
trace1 cs a clr = fst (h1 ((0.0, 0.0), 90.0) cs)
  where
    h1 :: TurtleState -> Commands -> ([ColouredLine], Commands)
    h1 _ [] = ([], [])
    h1 ts (c:cs)
      | c == ']' = ([], cs)
      | c == '[' = (intraces ++ outtraces, ununproc) -- processes commands in and out of the bracket, appending them together
      -- Normal commands
      | c == 'L' || c == 'R' = t 
      | c == 'F' = ((v1, v2, clr) : traces, csleft)
      where
        ts' = move c a ts
        (v1, _) = ts
        (v2, _) = ts'
        t@(traces, csleft) = h1 ts' cs 
        (intraces, unproc) =  h1 ts cs
        (outtraces, ununproc) = h1 ts unproc

trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 = h2 [] ((0.0, 0.0), 90.0)
  where
    -- Helper function for trace2
    -- Has 2 extra arguments: stack which helps store turtle states when entering a branch
    -- and ts which is the current turtle state
    h2 :: Stack -> TurtleState -> Commands -> Angle -> Colour -> [ColouredLine]
    h2 _ _ [] _ _ = []
    h2 stack ts (c:cs) a clr 
      | c == '[' = h2 (ts : stack) ts cs a clr -- adds current turtle state to top of stack
      | c == ']' = h2 stacktail top cs a clr -- fetches turtle state from top of stack and removing it
      -- Normal commands
      | c == 'L' || c == 'R' = traces
      | c == 'F' = (v1, v2, clr) : traces -- Only add a line when the turtle moves forward
      where
        ts' = move c a ts -- new turtle state
        (top : stacktail) = stack
        (v1, _) = ts
        (v2, _) = ts'
        traces = h2 stack ts' cs a clr


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
