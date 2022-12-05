module View (welcome) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, border)
import Brick.Widgets.Border.Style (unicodeBold, borderStyleFromChar)

import Model
import Model.Board
import Graphics.Vty hiding (dim)
import Model.Score

welcome :: PlayState -> [Widget String]
welcome s
  | psMode s == Intro        = drawIntro s
  | psMode s == Instruction  = drawInstruction
  | scMax (psScore s) == 0   = drawRounds
  | psMode s == Outro        = drawOutro s
  | psMode s == PlayEasy     = view s
  | psMode s == PlayUltimate = viewUltimate s
  | otherwise                = []

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
            optionString 3 currMode "2. Tic-Tac-Toe MinMax",
            optionString 4 currMode "3. Ultimate Tic-Tac-Toe"
      ]
    ]

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

drawInstruction :: [Widget String]
drawInstruction = [
  center
  $ withBorderStyle unicodeBold
  $ borderWithLabel (str "Tic-Tac-Toe!")
  $ vBox [padLeftRight 10 (str "Space Holder for Instructions ... ")]
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
viewUltimate s = [hBox [drawStats s,
      joinBorders $
      padLeftRight 5 $
      hLimit 45 $
      vLimit 90 $
      border $
      vBoard [mkBigRow s row | row <- [1..3]]
    ]
  ]

mkBigRow :: PlayState -> Int -> Widget n
mkBigRow s row = hBoard [mkSmallBoard s row i | i <- [1..3]]

mkSmallBoard :: PlayState -> Int -> Int -> Widget n
mkSmallBoard s bRow bCol = 
    hLimit 9 $
    vLimit 18 $
    vTile [mkSmallRow s row bCol | row <- [bRow*3-2 .. bRow*3]]

mkSmallRow :: PlayState -> Int -> Int -> Widget n
mkSmallRow s row bCol = 
  withBorderStyle (borderStyleFromChar '|') $ hTile [mkSmallCell s row i | i <- [bCol*3-2 .. bCol*3]]

mkSmallCell :: PlayState -> Int -> Int -> Widget n
mkSmallCell s r c
  | isCurr s r c = withCursor raw
  | otherwise    = raw
  where
    raw = center (mkSmallXO (gbBoard (psBoard s) ! Pos r c))

mkSmallXO :: Maybe XO -> Widget n
mkSmallXO Nothing  = smallBlockB
mkSmallXO (Just X) = smallBlockO
mkSmallXO (Just O) = smallBlockX

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
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
mkCell' s r c = center (mkXO xoMb)
  where
    xoMb      = gbBoard (psBoard s) ! Pos r c

mkXO :: Maybe XO -> Widget n
mkXO Nothing  = blockB
mkXO (Just X) = blockX
mkXO (Just O) = blockO

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

smallBlockB, smallBlockO, smallBlockX :: Widget n
smallBlockB = str "   "
smallBlockX = str " X "
smallBlockO = str " O "

vTile :: [Widget n] -> Widget n
vTile (b:bs) = withBorderStyle (borderStyleFromChar '-') $ vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = withBorderStyle (borderStyleFromChar '|') $ hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget

vBoard :: [Widget n] -> Widget n
vBoard (b:bs) = withBorderStyle (borderStyleFromChar '-') $ vBox (b : [hBorder <=> b | b <- bs])
vBoard _      = emptyWidget

hBoard :: [Widget n] -> Widget n
hBoard (b:bs) = withBorderStyle (borderStyleFromChar '|') $ hBox (b : [vBorder <+> b | b <- bs])
hBoard _      = emptyWidget