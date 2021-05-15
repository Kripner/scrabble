module Main where

import BoardData
import WordSearch hiding (Down)
import Utils

import Control.Applicative

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import qualified Data.Sequence as S

main :: IO ()
main = play (InWindow "Scrabble" windowSize (0, 0)) white 1 initialWorld drawWorld handleEvent updateWorld

updateWorld :: Float -> World -> World
updateWorld _ = id

-- Model

type Cursor = MatrixIndex
type World = (Board, Cursor)

initialWorld :: World
initialWorld = (initialBoard, (boardWidth // 2, boardHeight // 2))

-- Graphics

type Position = (Float, Float)
windowSize = (1200, 750)
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

-- Controls

handleEvent :: Event -> World -> World
handleEvent (EventKey key Down _ _) w@(board, cursor) = case key of
  SpecialKey k -> (board, handleUpdateCursor k cursor)
  Char c -> handleUpdateLetter c w
  _ -> w
handleEvent _ w = w

handleUpdateLetter :: Char -> World -> World
handleUpdateLetter c w@(board, cursor)
  | isValidInput c = (set board cursor (Character $ normalizeInput c), cursor)
  | c == '1' = w `debug` show (head (search board sampleHand sampleDict))
  | otherwise = w

handleUpdateCursor :: SpecialKey -> Cursor -> Cursor
handleUpdateCursor key (col, row) = (clamp 0 (boardWidth - 1) newCol, clamp 0 (boardHeight - 1) newRow)
  where
    (newCol, newRow) = case key of
      KeyLeft -> (col - 1, row)
      KeyUp -> (col, row + 1)
      KeyRight -> (col + 1, row)
      KeyDown -> (col, row - 1)
      _ -> (col, row)
