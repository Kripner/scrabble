module Graphics where

import BoardData

import Control.Applicative
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

type Position = (Float, Float)
windowSize = (1200, 750) :: (Int, Int)
tileSize = 30 :: Float
boardSize = (tileSize * fromIntegral boardWidth, tileSize * fromIntegral boardHeight)
charsScale = 0.2 :: Float

drawWorld :: World -> Picture
drawWorld (board, cursor) =
  Translate 0 100 $
  Pictures [
    drawBoard board,
    drawCursor cursor
  ]

indexToPosition :: MatrixIndex -> Position
indexToPosition (col, row) =
  (tileSize * fromIntegral col - (fst boardSize - tileSize) / 2,
   tileSize * fromIntegral row - (snd boardSize - tileSize) / 2)

drawBoard :: Board -> Picture
drawBoard board =
  Pictures [
      Color (makeColor 0.12 0.6 0.5 1) $ rectangleSolid (fst boardSize) (snd boardSize),
      Pictures $ liftA2 drawTile' [0 .. boardWidth - 1] [0 .. boardHeight - 1]
  ]
  where
    drawTile' col row = let pos = (col, row)
                        in drawTile (indexToPosition pos) (tiles `at` pos) (board `at` pos)

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
      Rotate 45 $ rectangleSolid (tileSize - 1) (tileSize - 1)
  ]
  where
    col DoubleLetter = makeColor 0.3 0.83 0.95 1
    col TripleLetter = makeColor 0.15 0.63 0.85 1
    col DoubleWord = light orange
    col TripleWord = red
    col CenterTile = yellow

drawChar :: Char -> Picture
drawChar c = Translate 30 10 $ Text [c]

drawCursor :: Cursor -> Picture
drawCursor pos =
  Translate x y $
  Color (makeColor 0.9 0.3 0.5 0.6) $
  rectangleSolid tileSize tileSize
  where (x, y) = indexToPosition pos
