module Main where

import Brick
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Model
import View 
import Control 
import System.Exit (exitSuccess)

-------------------------------------------------------------------------------
main :: IO ()
main = do
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty (Just chan) app Model.init
  exitSuccess

attributes :: AttrMap
attributes = attrMap defAttr [ (attrName "X", bg red)
                             , (attrName "T", bg white)
                             , (attrName "O", bg blue)
                             , (attrName "cursor", bg green)
                             , (attrName "candidates", bg yellow)
                             , (attrName "default", bg black)
                             , (attrName "Red", fg red)
                             , (attrName "Blue", fg blue)
                             ]

app :: App PlayState Tick String
app = App
  { appDraw         = welcome 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const attributes
  }