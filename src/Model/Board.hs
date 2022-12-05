{-# LANGUAGE DeriveFunctor #-}
module Model.Board
  ( -- * Types
    GameBoard (..)
  , XO (..)
  , Pos (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , positions
  , emptyPositions
  , boardWinner
  , flipXO

    -- * Moves
  , up
  , down
  , left
  , right
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------
data GameBoard = GB
  { gbBoard :: Board
  , gbDim :: Int
  }

type Board = M.Map Pos XO

data XO
  = X
  | O
  deriving (Eq, Show)

data Pos = Pos
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

(!) :: Board -> Pos -> Maybe XO
board ! pos = M.lookup pos board

dim :: GameBoard -> Int
dim = gbDim

positions :: GameBoard -> [Pos]
positions gb = [ Pos r c | r <- [1..dim gb], c <- [1..dim gb] ]

emptyPositions :: GameBoard -> [Pos]
emptyPositions gb  = [ p | p <- positions gb, M.notMember p (gbBoard gb)]

init :: Int -> GameBoard
init d = GB {
  gbBoard = M.empty,
  gbDim = d
  }

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------

data Result a
  = Draw
  | Win XO
  | Retry
  | Cont a
  deriving (Eq, Functor, Show)

put :: GameBoard -> XO -> Pos -> Result GameBoard
put gb xo pos = case M.lookup pos (gbBoard gb) of
  Just _  -> Retry
  Nothing -> result (gb {gbBoard = M.insert pos xo (gbBoard gb) })

result :: GameBoard -> Result GameBoard
result gb
  | isFull gb  = Draw
  | wins gb X  = Win  X
  | wins gb O  = Win  O
  | otherwise = Cont gb

wins :: GameBoard -> XO -> Bool
wins gb xo = or [ winsPoss (gbBoard gb) xo ps | ps <- winPositions (gbDim gb)]

winsPoss :: Board -> XO -> [Pos] -> Bool
winsPoss b xo ps = and [ b!p == Just xo | p <- ps ]

winPositions :: Int -> [[Pos]]
winPositions dim = rows dim ++ cols dim ++ diags dim

rows, cols, diags :: Int -> [[Pos]]
rows dim = [[Pos r c | c <- [1..dim]] | r <- [1..dim]]
cols dim = [[Pos r c | r <- [1..dim]] | c <- [1..dim]]
diags dim = [[Pos i i | i <- [1..dim]], [Pos i (dim+1-i) | i <- [1..dim]]]

isFull :: GameBoard -> Bool
isFull gb = M.size (gbBoard gb) == dim gb * dim gb

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: GameBoard -> Pos -> Pos
up _ p = p
  { pRow = max 1 (pRow p - 1)
  }

down :: GameBoard -> Pos -> Pos
down gb p = p
  { pRow = min (dim gb) (pRow p + 1)
  }

left :: GameBoard -> Pos -> Pos
left _ p = p
  { pCol   = max 1 (pCol p - 1)
  }

right :: GameBoard -> Pos -> Pos
right gb p = p
  { pCol = min (dim gb) (pCol p + 1)
  }

boardWinner :: Result a -> Maybe XO
boardWinner (Win xo) = Just xo
boardWinner _        = Nothing

flipXO :: XO -> XO
flipXO X = O
flipXO O = X

