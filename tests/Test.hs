module Main where

-- main :: IO ()
-- main = do 
--   putStrLn "\nRunning my tests... "
--   putStrLn "\nDone Testing"
--   exitSuccess

import Data.Bool (bool)
import System.Exit (exitFailure, exitSuccess)

-- import qualified Model
import qualified Model.Board as B
import qualified TestSuite
import Data.Map as M



testBoard1 = M.fromList [(B.Pos 1 1, B.X), (B.Pos 1 2, B.X), (B.Pos 1 3, B.X), (B.Pos 2 1, B.O)]

testBoard1Empty = [B.Pos 2 2, B.Pos 2 3, B.Pos 3 1, B.Pos 3 2, B.Pos 3 3]

-- emptyPositions testBoard1 == testBoard1Empty



main :: IO ()
main = do
  putStrLn "\nRunning my tests... "
  if B.emptyPositions testBoard1 == testBoard1Empty
    then putStrLn "TRUE"
  else putStrLn "FALSE"