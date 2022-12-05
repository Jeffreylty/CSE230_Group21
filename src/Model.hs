{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player
import Data.Map
-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data GameMode = Intro | Instruction | PlayEasy | PlayMinMax | PlayUltimate | Outro
  deriving (Eq)

selectiveModes :: Int
selectiveModes = 4

mapping :: Int -> GameMode
mapping i = fromList [(0, Intro), (1, Instruction), (2, PlayEasy), (3, PlayMinMax), (4, PlayUltimate), (5, Outro)] ! i

data PlayState = PS
  { psCurMode :: Int
  , psMode   :: GameMode
  , psX      :: Player.Player   -- ^ player X info
  , psO      :: Player.Player   -- ^ player O info
  , psScore  :: Score.Score     -- ^ current score
  , psBoard  :: Board.GameBoard     -- ^ current board
  , psTurn   :: Board.XO        -- ^ whose turn 
  , psPos    :: Board.Pos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result      
  } 

init :: PlayState
init = PS
  { psCurMode = 1
  , psMode   = Intro
  , psX      = Player.human
  , psO      = Player.rando
  , psScore  = Score.init 0
  , psBoard  = Board.init 3
  , psTurn   = Board.X
  , psPos    = Board.Pos {pRow = 1, pCol = 1}
  , psResult = Board.Cont ()
  }

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: PlayState -> Board.Result Board.GameBoard -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b'
                                  , psTurn  = Board.flipXO (psTurn s) })
next s res             = nextBoard s res 

nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
nextBoard s res = case res' of
                    Board.Win _ -> Left res' 
                    Board.Draw  -> Left res'
                    _           -> Right s' 
  where
    sc'  = Score.add (psScore s) (Board.boardWinner res) 
    res' = Score.winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = Board.init (Board.dim (psBoard s))  -- clear the board
             , psTurn  = Score.startPlayer sc' -- toggle start player
             } 