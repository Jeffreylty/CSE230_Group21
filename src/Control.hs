module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Model.Score
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
-- import Model.Player 

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev
  | psMode s == Intro = case ev of 
                          T.VtyEvent (V.EvKey V.KUp  _)        -> Brick.continue (selectUp s)
                          T.VtyEvent (V.EvKey V.KDown  _)      -> Brick.continue (selectDown s)
                          T.VtyEvent (V.EvKey V.KEnter  _)     -> Brick.continue (selectEnter s)
                          T.VtyEvent (V.EvKey (V.KChar '1') _) -> Brick.continue (chooseEasy s)
                          T.VtyEvent (V.EvKey (V.KChar '2') _) -> Brick.continue (chooseMinMax s)
                          T.VtyEvent (V.EvKey (V.KChar '3') _) -> Brick.continue (chooseUltimate s)
                          T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
                          _                               -> Brick.continue s
  | psMode s == Instruction = case ev of
                          T.VtyEvent (V.EvKey _  _)   -> Brick.continue Model.init
                          _                           -> Brick.continue s

  | scMax (psScore s) == 0 = case ev of
                          T.VtyEvent (V.EvKey (V.KChar d) _) -> Brick.continue (enterRounds d s)
                          T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
                          _                               -> Brick.continue s

  | psMode s == Outro = case ev of
                          T.VtyEvent (V.EvKey (V.KChar 'r') _) -> Brick.continue Model.init
                          T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
                          _                               -> Brick.continue s

  | otherwise = case ev of
                          AppEvent Tick                   -> nextS s =<< liftIO (play O s)
                          T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (play X s)
                          T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move (up (psBoard s))   s)
                          T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move (down (psBoard s))   s)
                          T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move (left (psBoard s))   s)
                          T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move (right (psBoard s))  s)
                          T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
                          _                               -> Brick.continue s

chooseEasy :: PlayState -> PlayState
chooseEasy s = s { psMode = PlayEasy}

chooseMinMax :: PlayState -> PlayState
chooseMinMax s = s { psMode = PlayMinMax}

chooseUltimate :: PlayState -> PlayState
chooseUltimate s = s { psMode = PlayUltimate, psBoard = Model.Board.init 9}

selectDown :: PlayState -> PlayState
selectDown s = s { psCurMode = min selectiveModes (psCurMode s + 1)}

selectUp :: PlayState -> PlayState
selectUp s = s { psCurMode = max 1 (psCurMode s - 1)}

selectEnter :: PlayState -> PlayState
selectEnter s 
  | x == PlayUltimate = s { psMode = x, psBoard = Model.Board.init 9}
  | otherwise = s { psMode = x}
    where x = mapping (psCurMode s)

enterRounds :: Char -> PlayState -> PlayState
enterRounds d s
  | d `elem` ['1'..'9'] = s {psScore = Model.Score.init (read [d] :: Int) }
  | otherwise = s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psPos = f (psPos s) }

-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result GameBoard)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = put2 (psBoard s) xo <$> getPos xo s 
  | otherwise      = return Retry

getPos :: XO -> PlayState -> IO Pos
getPos xo s = getStrategy xo s (psPos s) (psBoard s) xo

getStrategy :: XO -> PlayState -> Strategy 
getStrategy X s = plStrat (psX s)
getStrategy O s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result GameBoard -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s b of
  Right s' -> continue s'
  Left res ->
    Brick.continue (s { psResult = res, psMode = Outro, psScore = sc' })
    where
      sc' = Model.Score.add (psScore s) (Model.Board.boardWinner res)


