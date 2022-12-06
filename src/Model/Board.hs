{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
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
  , emptyPositionsRando
  , checkSmall2
  , to99
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
import Data.Maybe

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------
data GameBoard = GB
  { gbBoard :: Board
  , gbDim :: Int
  , gb33 :: Board
  , nextPos :: [Pos]
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

getNextValid :: GameBoard -> Pos -> [Pos]
getNextValid gb (Pos r0 c0)
  | isNothing (checkSmall2 (gb33 gb) (Pos (r0 - (((r0 -1) `div` 3) *3 )) (c0 - (((c0 -1) `div` 3) *3 )) )) = [ Pos (((r0 -1) `mod` 3) +1) (((c0 -1) `mod` 3) +1)]
  | otherwise = emptyPositions33 gb  -- if the 3*3 board of current move is not empty
                                    -- then add all empty positions in the 3*3 board


(!) :: Board -> Pos -> Maybe XO
board ! pos = M.lookup pos board

dim :: GameBoard -> Int
dim = gbDim

positions :: GameBoard -> [Pos]
positions gb = [ Pos r c | r <- [1..dim gb], c <- [1..dim gb] ]

positions33 :: [Pos]
positions33 = [ Pos r c | r <- [1..3], c <- [1..3] ]

emptyPositions :: GameBoard -> [Pos]
emptyPositions gb  = [ p | p <- positions gb, M.notMember p (gbBoard gb)]

emptyPositionsRando :: GameBoard ->[Pos]-> [Pos]
emptyPositionsRando gb pList = [ p | p <- pList , M.notMember p (gbBoard gb)]

to99 :: Pos -> [Pos]
to99 (Pos r c) = [ Pos (3*(r-1)+i) (3*(c-1)+j) | i <- [1..3], j <- [1..3] ]

emptyPositions33 :: GameBoard -> [Pos]
emptyPositions33 gb  = [ p | p <- positions33, M.notMember p (gb33 gb)]

init :: Int -> GameBoard
init d = GB {
  gbBoard = M.empty,
  gbDim = d,
  gb33 = M.empty,
  nextPos = []
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
        if convertPos pos `elem` nextPos gb then do
          let b' = M.insert pos xo (gbBoard gb)  -- insert the move into the larger board
          case sectorResult b' p2 of
            Nothing -> Cont gb{
              gbBoard = b',
              nextPos = getNextValid gb pos
              }
            Just _ ->
                result33 gb' {
                nextPos = getNextValid gb' pos
                }
                where gb' = gb {
                gbBoard = b',
                gb33 = M.insert p2 xo (gb33 gb)
                } -- insert the move into the 3*3 board and check if the 3*3 board is won
        else Retry

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
