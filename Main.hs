module Main where

import BoardData

import Control.Applicative

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import qualified Data.Sequence as S

main :: IO ()
main = play (InWindow "Scrabble" windowSize (0, 0)) white 1 initialWorld drawWorld handleEvent updateWorld

updateWorld :: Float -> World -> World
updateWorld _ = id

-- Model

type World = Board

initialWorld :: World
initialWorld = initialBoard

-- Graphics

windowSize = (1200, 750)
tileSize = 30 :: Float
boardSize = (tileSize * fromIntegral boardWidth, tileSize * fromIntegral boardHeight)
charsScale = 0.2 :: Float
type Position = (Float, Float)

drawWorld :: World -> Picture
drawWorld = drawBoard (0, 100)

drawBoard :: Position -> Board -> Picture
drawBoard (x, y) board =
  Translate x y $
  Pictures [
      Color (makeColor 0.12 0.6 0.5 1) $ rectangleSolid (fst boardSize) (snd boardSize),
      Translate (-(fst boardSize - tileSize) / 2) (-(snd boardSize - tileSize) / 2) $ Pictures $
          liftA2 drawTile' [0 .. boardWidth - 1] [0 .. boardHeight - 1]
  ]
  where
    drawTile' col row =
      drawTile (tileSize * fromIntegral col, tileSize * fromIntegral row)
               (tiles `at` (col, row))
               (board `at` (col, row))

drawTile :: Position -> TileType -> TileContent -> Picture
drawTile (x, y) tile content =
  Translate x y $
  Color black $
  Pictures [
      Color black $ rectangleWire tileSize tileSize,
      drawTileEffect tile,
      drawTileContent content
  ]

drawTileContent :: TileContent -> Picture
drawTileContent content = showContent content
  where
    showContent Empty = Blank
    showContent (Character c) =
      Pictures [
        Color white $ rectangleSolid (tileSize - 1) (tileSize - 1),
        Translate (-tileSize / 2) (-tileSize / 2) $ Scale charsScale charsScale $ drawChar c]


drawTileEffect :: TileType -> Picture
drawTileEffect Normal = Blank
drawTileEffect tileType =
  Color (col tileType) $
  Pictures [
      rectangleSolid (tileSize - 1) (tileSize - 1),
      --drawTriangle (-tileSize / 2, tileSize / 2) (tileSize / 2, tileSize / 2) (0, tileSize / 2 + triangleSize)
      Rotate 45 $ rectangleSolid (tileSize - 1) (tileSize - 1)
  ]
  where
    col DoubleLetter = makeColor 0.3 0.83 0.95 1
    col TripleLetter = makeColor 0.15 0.63 0.85 1
    col DoubleWord = light orange
    col TripleWord = red
    col CenterTile = yellow
    triangleSize = 8

drawTriangle :: Position -> Position -> Position -> Picture
drawTriangle a b c = Polygon [a, b, c]

drawChar :: Char -> Picture
drawChar c = Translate 30 10 $ Text [c]

-- Controls

handleEvent :: Event -> World -> World
handleEvent e w = w
