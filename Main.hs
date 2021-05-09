module Main where

import BoardData

import Control.Applicative

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import qualified Data.Sequence as S

main :: IO ()
main = play (InWindow "Scrabble" (750, 500) (0, 0)) white 1 initialWorld drawWorld handleEvent updateWorld

updateWorld :: Float -> World -> World
updateWorld _ = id

-- Model

type World = Board

initialWorld :: World
initialWorld = initialBoard

-- Graphics

tileSize = 30 :: Float
charsScale = 0.2 :: Float
type Position = (Float, Float)

drawWorld :: World -> Picture
drawWorld = drawBoard (0, 100)

drawBoard :: Position -> Board -> Picture
drawBoard (x, y) board =
  Translate x y $
  Pictures $
  liftA2 drawTile' [0 .. boardWidth - 1] [0 .. boardHeight - 1]
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
      drawTileBackground tile,
      Translate (-tileSize / 2) (-tileSize / 2) $ showContent content
  ]
  where
    showContent Empty = Blank
    showContent (Character c) = Scale charsScale charsScale $ drawChar c

drawTileBackground :: TileType -> Picture
drawTileBackground Normal = Color green $ rectangleSolid (tileSize - 1) (tileSize - 1)
drawTileBackground tileType =
  Color (col tileType) $
  Pictures [
      rectangleSolid (tileSize - 1) (tileSize - 1),
      --drawTriangle (-tileSize / 2, tileSize / 2) (tileSize / 2, tileSize / 2) (0, tileSize / 2 + triangleSize)
      Rotate 45 $ rectangleSolid (tileSize - 1) (tileSize - 1)
  ]
  where
    col _ = red
    triangleSize = 8

drawTriangle :: Position -> Position -> Position -> Picture
drawTriangle a b c = Polygon [a, b, c]

drawChar :: Char -> Picture
drawChar c = Translate 30 10 $ Text [c]

-- Controls

handleEvent :: Event -> World -> World
handleEvent e w = w
