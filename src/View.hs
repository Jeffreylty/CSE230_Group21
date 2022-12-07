module View (welcome) where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, border)
import Brick.Widgets.Border.Style (unicodeBold, unicodeRounded, unicode, BorderStyle(..))
import Data.Text (pack, Text)
import Model
import Model.Board
import Graphics.Vty hiding (dim)
import Model.Score

welcome :: PlayState -> [Widget String]
welcome s
  | psMode s == Intro        = drawIntro s
  | psMode s == Instruction  = drawInstruction
  | psOpponent s == None     = chooseWidget s
  | scMax (psScore s) == 0   = drawRounds
  | psMode s == Outro        = drawOutro s
  | psMode s == PlayUltimate = viewUltimate s
  | otherwise                = view s

drawIntro :: PlayState -> [Widget String]
drawIntro s = introWidget (psCurMode s)

introWidget :: Int -> [Widget String]
introWidget currMode = [
    center
    $ withBorderStyle unicodeBold
    $ borderWithLabel (str "Tic-Tac-Toe!")
    $ vBox [padLeftRight 10 (str "Welcome to Tic-Tac-Toe game!"),
            optionString 1 currMode "Game Instructions",
            padTop (Pad 2) $ padLeft (Pad 2) (str "Please Select the Game Mode:"),
            optionString 2 currMode "1. Simple Tic-Tac-Toe",
            optionString 3 currMode "2. Ultimate Tic-Tac-Toe"
      ]
    ]

chooseWidget :: PlayState -> [Widget String]
chooseWidget s 
  | psMode s == PlayEasy = [
    center
    $ hLimit 35
    $ withBorderStyle unicodeRounded
    $ borderWithLabel (str "Tic-Tac-Toe!")
    $ vBox [
      padTop (Pad 1) $ hCenter (str "Please Choose Your Opponent:"),
      optionString 1 (psCurMode s) "1. Smart AI",
      optionString 2 (psCurMode s) "2. MinMax AI",
      optionString 3 (psCurMode s) "3. Human"
    ]
  ]
  | psMode s == PlayUltimate = [
    center
    $ hLimit 35
    $ withBorderStyle unicodeRounded
    $ borderWithLabel (str "Tic-Tac-Toe!")
    $ vBox [
      padTop (Pad 1) $ hCenter (str "Please Choose Your Opponent:"),
      optionString 1 (psCurMode s) "1. Simple AI",
      optionString 2 (psCurMode s) "2. Human"
    ]
  ]
  | otherwise = []

optionString :: Int -> Int -> String -> Widget String
optionString n currMode s
  | n == currMode = vBox [str " ", modifyDefAttr (`withStyle` reverseVideo) $ padLeftRight 4 (str s)]
  | otherwise     = vBox [str " ", padLeftRight 4 (str s)]

drawOutro :: PlayState -> [Widget String]
drawOutro s
  | psResult s == Draw  = endingWidget "Tied!" (psScore s)
  | psResult s == Win X = endingWidget "Great, You Win!" (psScore s)
  | psResult s == Win O = endingWidget "Oops, You Lose..." (psScore s)
  | otherwise = []

endingWidget :: String -> Score -> [Widget String]
endingWidget s score = [
      center
      $ withBorderStyle unicodeBold
      $ borderWithLabel (str "Tic-Tac-Toe!")
      $ vBox [padLeftRight 10 (str (s ++ " (Press \"R\" to Retry)")),
              padLeftRight 10 (str ("Final Score is " ++ show (scX score) ++ " : " ++ show (scO score))),
              padTop (Pad 5) $ padLeft (Pad 15) (str "Press \"ESC\" to Exit")]
    ]

drawRounds :: [Widget String]
drawRounds = [
  center
  $ withBorderStyle unicodeBold
  $ borderWithLabel (str "Tic-Tac-Toe!")
  $ vBox [padLeftRight 10 (str "Please Enter Rounds to Play (1-9)")]
  ]

rules :: Text
rules = pack "Each small 3 × 3 tic-tac-toe board is referred to as a local board, and the larger 3 × 3 board is referred to as the global board.\n\nThe game starts with X playing wherever they want in any of the 81 empty spots. This move \"sends\" their opponent to its relative location. For example, if X played in the top right square of their local board, then O needs to play next in the local board at the top right of the global board. O can then play in any one of the nine available spots in that local board, each move sending X to a different local board.\n\nIf a move is played so that it is to win a local board by the rules of normal tic-tac-toe, then the entire local board is marked as a victory for the player in the global board. Once a local board is won by a player or it is filled completely, no more moves may be played in that board. If a player is sent to such a board, then that player may play in any other board. Game play ends when either a player wins the global board or there are no legal moves remaining, in which case the game is a draw"

drawInstruction :: [Widget String]
drawInstruction = [
  center
  $ withBorderStyle unicodeBold
  $ borderWithLabel (str "Tic-Tac-Toe!")
  $ vBox [hCenter $ padLeftRight 10 (str "Rules for Ultimate Tic-Tac-Toe"),
  hCenter $ padTop (Pad 1) $ padLeftRight 5 $ txtWrap rules 
  ]
  ]


-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s =
  joinBorders $
  withBorderStyle unicodeBold $ hBox
  [drawStats s,
    padLeft (Pad 5) $
    hLimit 60 $
    vLimit 60 $
    border $
      vTile [ mkRow s row | row <- [1..3] ]]

viewUltimate :: PlayState -> [Widget String]
viewUltimate s = [center $ hBox [drawStats s,
      padLeftRight 5 $
      hLimit 37 $
      vLimit 24 $
      withBorderStyle unicodeRounded $
      borderWithLabel (str "Game Board") $
      vBoard [mkBigRow s row | row <- [1..3]]
    ]
  ]


mkBigRow :: PlayState -> Int -> Widget n
mkBigRow s row = 
    vLimit 5 $
    hBoard [mkSmallBoard s row i | i <- [1..3]]

mkSmallBoard :: PlayState -> Int -> Int -> Widget n
mkSmallBoard s bRow bCol
  | Pos bRow bCol `elem` psNextBoard s = 
    hLimit 11 $
    vLimit 7 $
    withAttr (attrName "candidates") $ 
    vSmallTile [mkSmallRow s row bCol | row <- [bRow*3-2 .. bRow*3]]
  | Model.Board.checkSmall2 (gb33 (psBoard s)) (Pos bRow bCol) == Just X = 
    hLimit 11 $
    vLimit 7 $
    forceAttr (attrName "X") $
    vSmallTile [ mkSmallRow s row bCol | row <- [bRow*3-2 .. bRow*3]]
  | Model.Board.checkSmall2 (gb33 (psBoard s)) (Pos bRow bCol) == Just O = 
    hLimit 11 $
    vLimit 7 $
    forceAttr (attrName "O") $
    vSmallTile [mkSmallRow s row bCol | row <- [bRow*3-2 .. bRow*3]]
  | Model.Board.checkSmall2 (gb33 (psBoard s)) (Pos bRow bCol) == Just T = 
    hLimit 11 $
    vLimit 7 $
    forceAttr (attrName "T") $ 
    vSmallTile [mkSmallRow s row bCol | row <- [bRow*3-2 .. bRow*3]]
  | otherwise = 
    hLimit 11 $
    vLimit 7 $
    vSmallTile [withAttr (attrName "default") $ mkSmallRow s row bCol | row <- [bRow*3-2 .. bRow*3]]

mkSmallRow :: PlayState -> Int -> Int -> Widget n
mkSmallRow s row bCol =
  withBorderStyle unicode
  $ vLimit 1 
  $ hSmallTile [mkSmallCell s row i | i <- [bCol*3-2 .. bCol*3]]

mkSmallCell :: PlayState -> Int -> Int -> Widget n
mkSmallCell s r c
  | isCurr s r c = withCursor raw
  | otherwise    = raw
  where
    raw = hLimit 3 $ vLimit 1 $ center (mkSmallXO (gbBoard (psBoard s) ! Pos r c))

mkSmallXO :: Maybe XO -> Widget n
mkSmallXO Nothing  = smallBlockB
mkSmallXO (Just X) = smallBlockX
mkSmallXO (Just O) = smallBlockO
mkSmallXO _     = emptyWidget --dummy

drawStats :: PlayState -> Widget String
drawStats s =
  hLimit 30
    $ withBorderStyle unicodeBold
    $ borderWithLabel (str "Tic-Tac-Toe!")
    $ vBox
        [ drawString "Turn" turn
        , padTop (Pad 1) $ drawStat "X-Score" xscore
        , padTop (Pad 1) $ drawStat "O-Score" oscore
        , padTop (Pad 1) $ padLeftRight 1 $ str "Selected Grid:"
        , padTop (Pad 1) $ drawStat "Row" row
        , padTop (Pad 1) $ drawStat "Column" col
        , drawLeaderBoard
        ]
    where
        turn = show (psTurn s)
        row = pRow (psPos s)
        col = pCol (psPos s)
        xscore = scX (psScore s)
        oscore = scO (psScore s)


drawStat :: String -> Int -> Widget String
drawStat s n = padLeftRight 1 $ str s <+> padLeft Max (str $ show n)

drawString :: String -> String -> Widget String
drawString s x = padLeftRight 1 $ str s <+> padLeft Max (str $ show x)

drawLeaderBoard :: Widget String
drawLeaderBoard = emptyWidget

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [mkCell s row i | i <- [1..3]]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c
  | isCurr s r c = withCursor raw
  | otherwise    = raw
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = forceAttr (attrName "cursor")

mkCell' :: PlayState -> Int -> Int -> Widget n
mkCell' s r c = center (mkXO xoMb)
  where
    xoMb      = gbBoard (psBoard s) ! Pos r c

mkXO :: Maybe XO -> Widget n
mkXO Nothing  = blockB
mkXO (Just X) = blockX
mkXO (Just O) = blockO
mkXO (Just T) = emptyWidget -- dummy

blockB, blockX, blockO :: Widget n
blockB = vBox (replicate 5 (str "     "))
blockX = vBox [ str "X       X"
              , str "  X   X "
              , str "    X   "
              , str "  X   X "
              , str "X       X"]
blockO = vBox [ str "  OOOOO  "
              , str " O     O "
              , str "O       O"
              , str " O     O "
              , str "  OOOOO  "]

smallBlockB, smallBlockX, smallBlockO :: Widget n
smallBlockB = str "   "
smallBlockX = withAttr (attrName "Red") $ str  " X "
smallBlockO = withAttr (attrName "Blue") $ str " O "

vTile :: [Widget n] -> Widget n
vTile (b:bs) = withBorderStyle unicode $ vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = withBorderStyle unicode $ hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget

smallBoardStyle :: BorderStyle
smallBoardStyle = BorderStyle { bsIntersectFull = toEnum 0x253C
                    , bsCornerTL      = '+' , bsCornerTR      = '+'
                    , bsCornerBR      = '+' , bsCornerBL      = '+'
                    , bsIntersectL    = '+' , bsIntersectR    = '+'
                    , bsIntersectT    = '+' , bsIntersectB    = '+'
                    , bsHorizontal    = toEnum 0x2504 , bsVertical      = toEnum 0x2506
                    }

vSmallTile :: [Widget n] -> Widget n
vSmallTile (b:bs) = center
                    $ joinBorders
                    $ withBorderStyle smallBoardStyle
                    $ hLimit 11 $ vLimit 7
                    $ vBox (b : [hBorder <=> b | b <- bs])
vSmallTile _      = emptyWidget

hSmallTile :: [Widget n] -> Widget n
hSmallTile (b:bs) = center 
                    $ withBorderStyle smallBoardStyle
                    $ joinBorders
                    $ hLimit 11 $ vLimit 1 
                    $ hBox (b : [vBorder <+> b | b <- bs])
hSmallTile _      = emptyWidget

vBoard :: [Widget n] -> Widget n
vBoard (b:bs) = joinBorders 
                $ hLimit 37 $ vLimit 24 
                $ withBorderStyle unicode 
                $ vBox (b : [hBorder <=> b | b <- bs])
vBoard _      = emptyWidget

hBoard :: [Widget n] -> Widget n
hBoard (b:bs) = joinBorders 
                $ hLimit 37 $ vLimit 7 
                $ withBorderStyle unicode 
                $ hBox (b : [vBorder <+> b | b <- bs])
hBoard _      = emptyWidget