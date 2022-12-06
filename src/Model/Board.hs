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
  , put2
  , positions
  , emptyPositions
  , checkSmall2
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
  , gb33 :: Board
  }

type Board = M.Map Pos XO

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

-- converting the larger 9*9 board positions to the 3*3 section positions
convertPos :: Pos -> Pos
convertPos (Pos r c) = Pos ((r - 1) `div` 3 +1) ((c - 1) `div` 3 + 1)

-- //lookup for the 3*3 board
checkSmall2 :: Board -> Pos -> Maybe XO
checkSmall2 b pos = M.lookup pos b

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
  gbDim = d,
  gb33 = M.empty
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
-- | Winning cretiria for the larger 3*3 board
-------------------------------------------------------------------------------
put2 :: GameBoard -> XO -> Pos -> Result GameBoard
put2 gb xo pos = case M.lookup pos (gbBoard gb) of
  Just _  -> Retry
  Nothing -> do
    let p2 = convertPos pos         -- convert the position to the 3*3 board
    case checkSmall2 (gb33 gb) p2 of
      Just _ -> Retry              -- if the 3*3 board is already occupied, retry
      Nothing -> do
        let b' = M.insert pos xo (gbBoard gb)  -- insert the move into the larger board
        case sectorResult b' p2 of
          Nothing -> Cont gb{
            gbBoard = b'
            }
          Just xo -> do
            result33 gb{
            gbBoard = b',
            gb33 = M.insert p2 xo (gb33 gb)
            } -- insert the move into the 3*3 board and check if the 3*3 board is won

-- The final result is the result of the 3*3 board
result33 :: GameBoard -> Result GameBoard
result33 gb
  | isFull2 (gb33 gb)  = Draw
  | wins2 (gb33 gb) X  = Win  X
  | wins2 (gb33 gb) O  = Win  O
  | otherwise = Cont gb

wins2 :: Board -> XO -> Bool
wins2 b xo = or [ winsPoss2 b xo ps | ps <- winPositions2 ]

winsPoss2 :: Board -> XO -> [Pos] -> Bool
winsPoss2 b xo ps = and [ checkSmall2 b p  == Just xo | p <- ps ]

winPositions2 :: [[Pos]]
winPositions2 = rows2 ++ cols2 ++ diags2

rows2, cols2, diags2 :: [[Pos]]
rows2  = [[Pos r c | c <- [1..3]] | r <- [1..3]]
cols2  = [[Pos r c | r <- [1..3]] | c <- [1..3]]
diags2 = [[Pos i i | i <- [1..3]], [Pos i (3+1-i) | i <- [1..3]]]

isFull2 :: Board -> Bool
isFull2 b = M.size b == 3 * 3

-------------------------------------------------------------------------------
-- | Winning cretiria for each small 3*3 board
-------------------------------------------------------------------------------
sectorResult :: Board -> Pos -> Maybe XO
sectorResult b p
  | sectorIsFull b p = Just T
  | sectorWins b p X = Just X
  | sectorWins b p O = Just O
  | otherwise = Nothing

sectorWins :: Board -> Pos -> XO -> Bool
sectorWins b p xo = or [ sectorWinsPoss b xo ps | ps <- sectorWinPositions p ]

sectorWinsPoss :: Board -> XO -> [Pos] -> Bool
sectorWinsPoss b xo ps = and [ b!p  == Just xo | p <- ps ]

sectorWinPositions :: Pos -> [[Pos]]
sectorWinPositions ps = sectorRows ps ++ sectorCols ps ++ sectorDiags ps

sectorRows :: Pos -> [[Pos]]
sectorRows (Pos r0 c0) = [[Pos r c | c <- [((c0-1)*3+1) .. (c0*3)]] | r <- [((r0-1)*3+1) .. (r0*3)]]
sectorCols :: Pos -> [[Pos]]
sectorCols  (Pos r0 c0) = [[Pos r c | r <- [((r0-1)*3+1) .. (r0*3)]] | c <- [((c0-1)*3+1) .. (c0*3)]]
sectorDiags :: Pos -> [[Pos]]
sectorDiags (Pos r0 c0) = [[Pos ((r0-1)*3 + i) ((c0 - 1 )*3 + i) | i <- [1..3]], 
                                    [Pos ((r0-1)*3 + i) ((c0 - 1 )*3 + 4 - i) | i <- [1..3]]]


sectorIsFull :: Board -> Pos -> Bool
sectorIsFull b ps = and [ b!p == Just X || b!p == Just O | p <- sectorPositions ps] 

sectorPositions :: Pos -> [Pos]
sectorPositions (Pos r0 c0) = [ Pos r c | r <- [((r0-1)*3+1) .. (r0*3)], c <- [((c0-1)*3+1) .. (c0*3)]]


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
flipXO T = T -- dummy
