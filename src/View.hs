module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, border)
import Brick.Widgets.Border.Style (unicodeBold,unicode,unicodeRounded, borderStyleFromChar)

import Model
import Model.Board
import Graphics.Vty hiding (dim)
import Model.Score

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicodeBold $ hBox 
  [drawStats s, 
    vBox [
          padLeft (Pad 5) $
          hLimit 20 $
          vLimit 16 $
          border $
          withBorderStyle (borderStyleFromChar '-') $
          vTile [mkRow1 s row | row <- [1..3]],
          padLeft (Pad 5) $
          hLimit 20 $
          vLimit 16 $
          border $
          withBorderStyle (borderStyleFromChar '-') $
          vTile [mkRow1 s row | row <- [4..6]],
          padLeft (Pad 5) $
          hLimit 20 $
          vLimit 16 $
          border $
          withBorderStyle (borderStyleFromChar '-') $
          vTile [mkRow1 s row | row <- [7..9]]
    ],
        vBox [
          hLimit 20 $
          vLimit 16 $
          border $
          withBorderStyle (borderStyleFromChar '-') $
          vTile [mkRow2 s row | row <- [1..3]],
          hLimit 20 $
          vLimit 16 $
          border $
          withBorderStyle (borderStyleFromChar '-') $
          vTile [ mkRow2 s row | row <- [4..6]],
          hLimit 20 $
          vLimit 16 $
          border $
          withBorderStyle (borderStyleFromChar '-') $
          vTile [mkRow2 s row | row <- [7..9]]
    ],
        vBox [
          hLimit 20 $
          vLimit 16 $
          border $
          withBorderStyle (borderStyleFromChar '-') $
          vTile [mkRow3 s row | row <- [1..3]],
          hLimit 20 $
          vLimit 16$
          border $
          withBorderStyle (borderStyleFromChar '-') $
          vTile [mkRow3 s row | row <- [4..6]],
          hLimit 20 $
          vLimit 16 $
          border $
          withBorderStyle (borderStyleFromChar '-') $
          vTile [mkRow3 s row | row <- [7..9]]
    ]
      ]
    
      

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

-- header :: PlayState -> String
-- header s = printf "Tic-Tac-Toe Turn = %s, row = %d, col = %d" (show (psTurn s)) (pRow p) (pCol p)
--   where 
--     p    = psPos s

mkRow1 :: PlayState -> Int -> Widget n
mkRow1 s row = withBorderStyle (borderStyleFromChar '|') $ hTile [ mkCell s row i | i <- [1..3] ]

mkRow2 :: PlayState -> Int -> Widget n
mkRow2 s row = withBorderStyle (borderStyleFromChar '|') $ hTile [ mkCell s row i | i <- [4..6] ]

mkRow3 :: PlayState -> Int -> Widget n
mkRow3 s row = withBorderStyle (borderStyleFromChar '|') $ hTile [ mkCell s row i | i <- [7..9] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s r c = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkXO xoMb)
  where 
    xoMb      = psBoard s ! Pos r c
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkXO :: Maybe XO -> Widget n
mkXO Nothing  = blockB
mkXO (Just X) = blockX
mkXO (Just O) = blockO

blockB, blockX, blockO :: Widget n
blockB = vBox (replicate 3 (str "     "))
blockX = vBox [ str " X   X"
              , str "   X  "
              , str " X   X" ]
blockO = vBox [ str " O O "
              , str "O   O"
              , str " O O "]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget