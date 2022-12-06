module Model.Player where

import Model.Board ( Pos(Pos), XO(..), Board, (!), emptyPositions )
import System.Random -- (Random(randomRIO))
import Data.List
import Debug.Trace

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player 
  { plName  :: String 
  , plStrat :: Strategy
  } 

type Strategy = Pos     -- ^ current cursor
             -> Board   -- ^ current board
             -> XO      -- ^ naught or cross
             -> IO Pos  -- ^ next move

human :: Player 
human = Player "human" (\p _ _ -> return p)

rando :: Player
rando = Player "machine" aceStrategy

randomStrategy :: a -> Board -> b -> IO Pos
randomStrategy _ b _ = selectRandom (emptyPositions b) 

selectRandom :: [a] -> IO a
selectRandom xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)


aceStrategy :: a -> Board -> b -> IO Pos
aceStrategy _ b _ = do
  if  (b ! (Pos 2 2) /= Just X) &&  (b ! (Pos 2 2) /= Just O)
    then return (Pos 2 2)
  else if b ! (Pos 1 1) == Just X && b ! (Pos 1 3) == Just X && (Pos 1 2) `elem`(emptyPositions b)
    then return (Pos 1 2)
  else if b ! (Pos 1 1) == Just X && b ! (Pos 1 2) == Just X && (Pos 1 3) `elem`(emptyPositions b)
    then return (Pos 1 3)
  else if b ! (Pos 1 1) == Just X && b ! (Pos 3 1) == Just X && (Pos 2 1) `elem`(emptyPositions b)
    then return (Pos 2 1)
  else if b ! (Pos 1 1) == Just X && b ! (Pos 2 1) == Just X && (Pos 3 1) `elem`(emptyPositions b)
    then return (Pos 3 1)
  else if b ! (Pos 2 1) == Just X && b ! (Pos 3 1) == Just X && (Pos 1 1) `elem`(emptyPositions b)
    then return (Pos 1 1)
  else if b ! (Pos 3 1) == Just X && b ! (Pos 3 2) == Just X && (Pos 3 3) `elem`(emptyPositions b)
    then return (Pos 3 3)
  else if b ! (Pos 3 1) == Just X && b ! (Pos 3 3) == Just X && (Pos 3 2) `elem`(emptyPositions b)
    then return (Pos 3 2)
  else if b ! (Pos 3 2) == Just X && b ! (Pos 3 3) == Just X && (Pos 3 1) `elem`(emptyPositions b)
    then return (Pos 3 1)
  else if b ! (Pos 1 3) == Just X && b ! (Pos 2 3) == Just X && (Pos 3 3) `elem`(emptyPositions b)
    then return (Pos 3 3)
  else if b ! (Pos 2 3) == Just X && b ! (Pos 3 3) == Just X && (Pos 1 3) `elem`(emptyPositions b)
    then return (Pos 1 3)
  else if b ! (Pos 1 3) == Just X && b ! (Pos 3 3) == Just X && (Pos 2 3) `elem`(emptyPositions b)
    then return (Pos 2 3)
  else 
    selectRandom (emptyPositions b) 


minMaxStrategy :: a -> Board -> b -> IO Pos
minMaxStrategy _ b _ = do
  let board = convertBoard b
  let p = XX
  let tree = makeTree' board p
  let afterTree = minimax tree 3
  return (getMove tree afterTree)

convertBoard :: Board -> Board'
convertBoard b = (fillBoard b, 3)

for :: [a] -> (a -> b) -> [b]
for = flip map


fillBoard :: Board -> [Cell]
fillBoard b = concat (
    for [1..3] (\i -> do
      for [1..3] (\j -> do
        let pos = Pos i j
        
        if (b ! pos) ==  Just X
          then Taken XX
        else if (b ! pos) == Just O
          then Taken OO
        else Empty
        )
      ))

getMove :: Tree -> Tree -> Pos
getMove b a = Pos posi posj
    where 
      posi = div pos 3 + 1 
      posj = mod pos 3 + 1
      pos = getPoss bbcells aacells 8
      bb@(bbcells, _) = treeBoard b
      aa@(aacells, _) = treeBoard a


getPoss :: Eq a => [a] -> [a] -> Int -> Int
getPoss bcells acells n = 
  if bcells !! n == acells !! n
    then getPoss bcells acells (n-1)
  else n 


data Player' = OO | XX
            deriving (Eq, Show, Read)

data Cell = Empty | Taken Player'
          deriving (Eq)

instance Show Cell where
  show Empty      = "-"
  show (Taken p)  = show p

type Board' = ([Cell], Int)

type Position = Int

-------------------------------------------------------------------
-- Utility functions

-- Returns the next player in turn
player :: Player' -> Player'
player XX = OO
player OO = XX

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe _ _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace n p ls
  = before ++ (p : after)
    where
      (before, (_ : after)) = splitAt n ls

-- Returns the rows of a given board.
rows :: Board' -> [[Cell]]
rows (cs, n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board' -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board' -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]



data Tree = Node Board' Player' Bool Util [(Position, Tree)]

-- Generates the tree given the board size and starting player
makeTree' :: Board' -> Player' -> Tree
makeTree' b@(cells, _) p'
        = Node b p' term util (map makeChildTree (actions b))
          where
            (term, util)  = terminalUtility b (player p')
            makeChildTree :: Position -> (Position, Tree)
            makeChildTree pos
              = (pos, makeTree' (replace pos (Taken p') cells, 3) (player p'))

-------------------------------------------------------------------
-- Helper function to access the elements in the tree

treeBoard :: Tree -> Board'
treeBoard (Node b _ _ _ _)
  = b

treePlayer :: Tree -> Player'
treePlayer (Node _ p _ _ _)
  = p

treeTerminal :: Tree -> Bool
treeTerminal (Node _ _ term _ _)
  = term

treeUtility :: Tree -> Util
treeUtility (Node _ _ _ util _)
  = util

treeChildren :: Tree -> [Tree]
treeChildren (Node _ _ _ _ ts)
  = [t | (_, t) <- ts]

-------------------------------------------------------------------
-- Helpful type definitions and constants

type Util = Int
type Level = Int
type AgentState = (Tree, Util)

-------------------------------------------------------------------
-- Minimax agent helper functions

-- Returns a list of all possible actions that can be taken given
-- the current state of a board
actions :: Board' -> [Position]
actions (cells, _)
  = [i | (c, i) <- zip cells [0..], c == Empty]

validMove :: Position -> Board' -> Maybe Position
validMove pos (cells, n)
  | pos < 0 || (n * n) <= pos = Nothing
  | cells !! pos /= Empty     = Nothing
  | otherwise                 = Just pos

result :: Player' -> Position -> Board' -> Maybe Board'
result p pos b@(cells, n)
  = do
      pos' <- validMove pos b
      let cells' = replace pos' (Taken p) cells
      return (cells', n)

-- Helper method for calculating terminal and utility
-- Created to calculate util only once per terminal check
terminalUtility :: Board' -> Player' -> (Bool, Util)
terminalUtility b@(cells, n) p
  = (isFull cells || abs utilP == n || abs utilP' == n, utilP)
    where
      utilP   = util p
      utilP'  = util (player p)
      isFull :: [Cell] -> Bool
      isFull
        = not . (elem Empty)
      util :: Player' -> Util
      util p'
        = find $ map (util' 0) $ concat [f b | f <- [rows, cols, diags]]
          where
            (find, op)
              = case p' of  XX -> (maximum, (+))
                            OO -> (minimum, (-))
            opp' :: Player'
            opp'
              = player p'
            util' :: Util -> [Cell] -> Util
            util' u []
              = u
            util' u ((Taken p'') : cs)
              | p'' == opp' = 0
              | otherwise     = util' (u `op` 1) cs
            util' u (Empty : cs)
              = util' u cs

-- Returns true if game is over, false otherwise
terminal :: Board' -> Player' -> Bool
terminal
  = (fst .) . terminalUtility

-- Given a player, returns the maximum number of uninterrupted squares
-- X max player (value +ve) and O min player (value -ve)
utility :: Board' -> Player' -> Util
--Pre: b must be in a terminal state
utility
  = (snd .) . terminalUtility

-- Get the maximum/minimum bound for utility depending on player
utilBound :: Player' -> Int -> Util
utilBound XX n
  = n + 1
utilBound OO n
  = -(n + 1)

-------------------------------------------------------------------
-- Minimax agent

-- Returns the subtree after the minimax AI agent decided on its best move
minimax :: Tree -> Level -> Tree
minimax t lvlMax
--Pre: lvlMax > 0
  = t'
    where
      (_, n)  = treeBoard t
      (t', _) = minimax' t (utilBound (treePlayer t) n) 0
      minimax' :: Tree -> Util -> Level -> AgentState
      minimax' t prevBound lvl
        -- Depth-limit search
        | lvl >= lvlMax   = (t, treeUtility t)
        | treeTerminal t  = (t, treeUtility t)
        | otherwise       = bestBoard (treeChildren t) unitState prevBound
          where
            p = treePlayer t
            (comp, comp') = case p of XX -> ((>), min)
                                      OO -> ((<), max)
            unitState :: AgentState
            unitState
              = (t, utilBound (player p) n)
            bestBoard :: [Tree] -> AgentState -> Util -> AgentState
            bestBoard [] as _
              = as
            bestBoard (t1 : ts) as@(_, u) prevBound
              = bestBoard ts newState (comp' prevBound u')
                where
                  (_, u') = minimax' t1 prevBound (lvl + 1)
                  newState
                    | u `comp` prevBound  = as
                    | u' `comp` u         = (t1, u')
                    | otherwise           = as
