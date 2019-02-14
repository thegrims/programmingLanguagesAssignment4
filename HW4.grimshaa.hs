module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--

cmdToState :: (State, Maybe Line) -> State
cmdToState (state,line) = state

cmdToMaybeLine :: (State, Maybe Line) -> Maybe Line
cmdToMaybeLine (state, line) = line

cmdToLine :: (State, Maybe Line) -> Line
cmdToLine (state, (Just line)) = line

lineToPoint :: Line -> Point
lineToPoint (first,second) = second

cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen mode) (_, (x, y)) = ((mode, (x, y)), Nothing)
cmd (Move x y) (Up, (_, _)) = ((Up, (x, y)), Nothing)
cmd (Move x y) (Down, (x2, y2)) = ((Down, (x, y)), (Just ((x2, y2), (x, y))))

-- cmd (Pen mode) = \state -> (state,Nothing)

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog progN state =
    ((Down, (getLast list)), list)
    where {list = lineM progN state; }

lineM :: Prog -> State -> [Line]
lineM [] = \state -> []
lineM (command:restOfList) = \state -> 
    if (cmdToMaybeLine (cmd command state)) == Nothing then lineM restOfList (cmdToState (cmd command state))
    else [(cmdToLine (cmd command state))] ++ (lineM restOfList (cmdToState (cmd command state)))

getLast :: [Line] -> Point
getLast list = lineToPoint (last list)
--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = house 2 2 ++ house 15 2 ++ house 28 2 ++ house 10 15 ++ house 23 15

house :: Int -> Int -> Prog
house x y = wall x y ++ wall (x+2) y ++ wall (x+8) y ++ wall (x+10) y ++ steps 6 x (y+6) ++ stepsDown 6 (x+6) (y+12) ++ box (x+4) (y+4) ++ box (x+6) (y+4)

wall :: Int -> Int -> Prog 
wall x y = box x y ++ box x (y+2) ++ box x (y+4)

stepsDown :: Int -> Int -> Int -> Prog
stepsDown n x y = [Pen Up, Move x y, Pen Down] ++ step n
  where 
    step 0 = []
    step n = step (n-1) ++ [Move (x+n-1) (y-n), Move (x+n) (y-n)]