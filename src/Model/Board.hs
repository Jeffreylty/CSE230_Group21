{-# LANGUAGE DeriveFunctor #-}
module Model.Board
  ( -- * Types
    Board
  , Board2
  , XO (..)
  , Pos (..)
  , PosSmall (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , put2
  , positions
  , emptyPositions
  , boardWinner
  , flipXO
  , init2
  , convertPos

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

type Board = M.Map Pos XO

type Board2 = M.Map PosSmall XO

data XO
  = X
  | O
  | T
  deriving (Eq, Show)

data Pos = Pos
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

-- the larger 3*3 board positions
data PosSmall = PosSmall
  { pRowSmall :: Int  -- 1 <= pRowSmall <= 3
  , pColSmall :: Int  -- 1 <= pColSmall <= 3
  }
  deriving (Eq, Ord)

-- converting the larger 9*9 board positions to the 3*3 section positions
convertPos :: Pos -> PosSmall
convertPos (Pos r c) = PosSmall ((r - 1) `div` 3 +1) ((c - 1) `div` 3 + 1)

(!) :: Board -> Pos -> Maybe XO
board ! pos = M.lookup pos board

-- //lookup for the 3*3 board
checkSmall :: Board2 -> Pos -> Maybe XO
checkSmall b pos = M.lookup (convertPos pos) b

checkSmall2 :: Board2 -> PosSmall -> Maybe XO
checkSmall2 b pos = M.lookup pos b

dim :: Int
dim = 9

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ]

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- positions, M.notMember p board]

init :: Board
init = M.empty

init2 :: Board2
init2 = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------

data Result a 
  = Draw
  | Win XO
  | Retry
  | Cont a
  deriving (Eq, Functor, Show)

put :: Board -> XO -> Pos -> Result Board
put board xo pos = case M.lookup pos board of
  Just _  -> Retry
  Nothing -> result (M.insert pos xo board)

result :: Board -> Result Board
result b
  | isFull b  = Draw
  | wins b X  = Win  X
  | wins b O  = Win  O
  | otherwise = Cont b 

wins :: Board -> XO -> Bool
wins b xo = or [ winsPoss b xo ps | ps <- winPositions ]

winsPoss :: Board -> XO -> [Pos] -> Bool
winsPoss b xo ps = and [ b!p == Just xo | p <- ps ]

winPositions :: [[Pos]]
winPositions = rows ++ cols ++ diags

rows, cols, diags :: [[Pos]]
rows  = [[Pos r c | c <- [1..dim]] | r <- [1..dim]]
cols  = [[Pos r c | r <- [1..dim]] | c <- [1..dim]]
diags = [[Pos i i | i <- [1..dim]], 
                                    [Pos i (dim+1-i) | i <- [1..dim]]]

isFull :: Board -> Bool
isFull b = M.size b == dim * dim

-------------------------------------------------------------------------------
-- | Winning cretiria for the larger 3*3 board
-------------------------------------------------------------------------------
put2 :: Board -> Board2 -> XO -> Pos -> Result Board
put2 board b2 xo pos = case M.lookup pos board of
  Just _  -> Retry
  Nothing -> do
    let p2 = convertPos pos         -- convert the position to the 3*3 board
    case checkSmall2 b2 p2 of
      Just _ -> Retry              -- if the 3*3 board is already occupied, retry
      Nothing -> do
        let b' = M.insert pos xo board  -- insert the move into the larger board
        case sectorResult b' p2 of
          Nothing -> Cont b'
          Just xo -> do
            result33 (M.insert p2 xo b2) b' -- insert the move into the 3*3 board and check if the 3*3 board is won

-- The final result is the result of the 3*3 board
result33 :: Board2 -> Board -> Result Board 
result33 b2 b
  | isFull2 b2  = Draw
  | wins2 b2 X  = Win  X
  | wins2 b2 O  = Win  O
  | otherwise = Cont b 

wins2 :: Board2 -> XO -> Bool
wins2 b xo = or [ winsPoss2 b xo ps | ps <- winPositions2 ]

winsPoss2 :: Board2 -> XO -> [PosSmall] -> Bool
winsPoss2 b xo ps = and [ checkSmall2 b p  == Just xo | p <- ps ]

winPositions2 :: [[PosSmall]]
winPositions2 = rows2 ++ cols2 ++ diags2

rows2, cols2, diags2 :: [[PosSmall]]
rows2  = [[PosSmall r c | c <- [1..3]] | r <- [1..3]]
cols2  = [[PosSmall r c | r <- [1..3]] | c <- [1..3]]
diags2 = [[PosSmall i i | i <- [1..3]], 
                                    [PosSmall i (3+1-i) | i <- [1..3]]]

isFull2 :: Board2 -> Bool
isFull2 b = M.size b == 3 * 3

-------------------------------------------------------------------------------
-- | Winning cretiria for each small 3*3 board
-------------------------------------------------------------------------------
sectorResult :: Board -> PosSmall -> Maybe XO
sectorResult b p
  | sectorIsFull b p = Just T
  | sectorWins b p X = Just X
  | sectorWins b p O = Just O
  | otherwise = Nothing

sectorWins :: Board -> PosSmall -> XO -> Bool
sectorWins b p xo = or [ sectorWinsPoss b xo ps | ps <- sectorWinPositions p ]

sectorWinsPoss :: Board -> XO -> [Pos] -> Bool
sectorWinsPoss b xo ps = and [ b!p  == Just xo | p <- ps ]

sectorWinPositions :: PosSmall -> [[Pos]]
sectorWinPositions ps = sectorRows ps ++ sectorCols ps ++ sectorDiags ps

sectorRows :: PosSmall -> [[Pos]]
sectorRows (PosSmall r0 c0) = [[Pos r c | c <- [((c0-1)*3+1) .. (c0*3)]] | r <- [((r0-1)*3+1) .. (r0*3)]]
sectorCols :: PosSmall -> [[Pos]]
sectorCols  (PosSmall r0 c0) = [[Pos r c | r <- [((r0-1)*3+1) .. (r0*3)]] | c <- [((c0-1)*3+1) .. (c0*3)]]
sectorDiags :: PosSmall -> [[Pos]]
sectorDiags (PosSmall r0 c0) = [[Pos ((r0-1)*3 + i) ((c0 - 1 )*3 + i) | i <- [1..3]], 
                                    [Pos ((r0-1)*3 + i) ((c0 - 1 )*3 + 4 - i) | i <- [1..3]]]


sectorIsFull :: Board -> PosSmall -> Bool
sectorIsFull b ps = and [ b!p == Just X || b!p == Just O | p <- sectorPositions ps] 

sectorPositions :: PosSmall -> [Pos]
sectorPositions (PosSmall r0 c0) = [ Pos r c | r <- [((r0-1)*3+1) .. (r0*3)], c <- [((c0-1)*3+1) .. (c0*3)]]

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: Pos -> Pos
up p = p
  { pRow = max 1 (pRow p - 1)
  }

down :: Pos -> Pos
down p = p
  { pRow = min dim (pRow p + 1)
  }

left :: Pos -> Pos
left p = p
  { pCol   = max 1 (pCol p - 1)
  }

right :: Pos -> Pos
right p = p
  { pCol = min dim (pCol p + 1)
  }

boardWinner :: Result a -> Maybe XO
boardWinner (Win xo) = Just xo
boardWinner _        = Nothing

flipXO :: XO -> XO
flipXO X = O
flipXO O = X

