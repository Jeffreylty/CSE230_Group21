module Main where
import Data.Map as M
import Model.Board as B
import System.Exit


-- Test Board 1
-- X | X | O
-- X | O | O
--   |   | 

board1 = M.fromList [(B.Pos 1 1, B.X), (B.Pos 1 2, B.X), (B.Pos 1 3, B.O), (B.Pos 2 1, B.X), (B.Pos 2 2, B.O), (B.Pos 2 3, B.O)]
gameBoard1 = GB board1 3 M.empty []
board1Empty = [B.Pos 3 1, B.Pos 3 2, B.Pos 3 3]

-- put X to 3 1
-- X | X | O
-- X | O | O  
-- X |   | 
board1OneStep = M.fromList [(B.Pos 1 1, B.X), (B.Pos 1 2, B.X), (B.Pos 1 3, B.O), (B.Pos 2 1, B.X), (B.Pos 2 2, B.O), (B.Pos 2 3, B.O), (B.Pos 3 1, B.X)]
gameBoard1OneStep = GB board1OneStep 3 M.empty []


test1 :: IO ()
test1 = do
  putStrLn "\nRunning Test 1..."
  if B.emptyPositions gameBoard1 == board1Empty
    then putStrLn "Correct! Empty posistions"
  else putStrLn "Incorrect! Empty posistions"
  if B.wins gameBoard1OneStep B.X
    then putStrLn "Correct! X wins"
  else putStrLn "Incorrect! X wins"
  if B.wins gameBoard1OneStep B.O
    then putStrLn "Incorrect! O does not win"
  else putStrLn "Correct! O does not win"
  if B.isFull gameBoard1OneStep
    then putStrLn "Incorrect! GameBoard now is not full"
  else putStrLn "Correct! GameBoard now is not full"
  if M.insert (B.Pos 3 1) B.X (B.gbBoard gameBoard1) == B.gbBoard gameBoard1OneStep
    then putStrLn "Correct! Put X to position 3 1"
  else putStrLn "Incorrect! Put X to position 3 1"




-- Test Board 2
-- O |   | 
--   | X |  
--   |   | 
board2 = M.fromList [(B.Pos 1 1, B.O), (B.Pos 2 2, B.X)]
gameBoard2 = GB board2 3 M.empty []
board2Empty = [B.Pos 1 2, B.Pos 1 3, B.Pos 2 1, B.Pos 2 3, B.Pos 3 1, B.Pos 3 2, B.Pos 3 3]

-- put O to 1 2
-- O | O | 
--   | X |  
--   |   | 
board2OneStep = M.fromList [(B.Pos 1 1, B.O), (B.Pos 1 2, B.O), (B.Pos 2 2, B.X)]
gameBoard2OneStep = GB board2OneStep 3 M.empty []


test2 :: IO ()
test2 = do
  putStrLn "\nRunning Test 2..."
  if B.emptyPositions gameBoard2 == board2Empty
    then putStrLn "Correct! Empty posistions"
  else putStrLn "Incorrect! Empty posistions"
  if B.wins gameBoard2OneStep B.X
    then putStrLn "Incorrect! X does not win"
  else putStrLn "Correct! X does not win"
  if B.wins gameBoard2OneStep B.O
    then putStrLn "Incorrect! O does not win"
  else putStrLn "Correct! O does not win"
  if B.isFull gameBoard2OneStep
    then putStrLn "Incorrect! GameBoard now is not full"
  else putStrLn "Correct! GameBoard now is not full"
  if M.insert (B.Pos 1 2) B.O (B.gbBoard gameBoard2) == B.gbBoard gameBoard2OneStep
    then putStrLn "Correct! Put O to position 1 2"
  else putStrLn "Incorrect! Put O to position 1 2"


-- Test Board 3
-- O | X | X
-- X | X | O 
-- O | O | 
board3 = M.fromList [(B.Pos 1 1, B.O), (B.Pos 1 2, B.X), (B.Pos 1 3, B.X), (B.Pos 2 1, B.X), (B.Pos 2 2, B.X), (B.Pos 2 3, B.O), (B.Pos 3 1, B.O), (B.Pos 3 2, B.O)]
gameBoard3 = GB board3 3 M.empty []
board3Empty = [B.Pos 3 3]

-- put X to 3 3
-- O | X | X
-- X | X | O 
-- O | O | X
board3OneStep = M.fromList [(B.Pos 1 1, B.O), (B.Pos 1 2, B.X), (B.Pos 1 3, B.X), (B.Pos 2 1, B.X), (B.Pos 2 2, B.X), (B.Pos 2 3, B.O), (B.Pos 3 1, B.O), (B.Pos 3 2, B.O), (B.Pos 3 3, B.X)]
gameBoard3OneStep = GB board3OneStep 3 M.empty []


test3 :: IO ()
test3 = do
  putStrLn "\nRunning Test 3..."
  if B.emptyPositions gameBoard3 == board3Empty
    then putStrLn "Correct! Empty posistions"
  else putStrLn "Incorrect! Empty posistions"
  if B.wins gameBoard3OneStep B.X
    then putStrLn "Incorrect! X does not win"
  else putStrLn "Correct! X does not win"
  if B.wins gameBoard3OneStep B.O
    then putStrLn "Incorrect! O does not win"
  else putStrLn "Correct! O does not win"
  if B.isFull gameBoard3OneStep
    then putStrLn "Correct! GameBoard is now full"
  else putStrLn "Incorrect! GameBoard is now full"
  if M.insert (B.Pos 3 3) B.X (B.gbBoard gameBoard3) == B.gbBoard gameBoard3OneStep
    then putStrLn "Correct! Put X to position 3 3"
  else putStrLn "Incorrect! Put X to position 3 3"


main :: IO ()
main = do
  putStrLn "\nRunning my tests... "
  test1
  test2
  test3
  putStrLn "\nDone Testing"
  exitSuccess
