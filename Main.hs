module Main where

import BoardData
import WordSearch hiding (Down)
import Graphics
import Utils

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import qualified Data.Sequence as S

main :: IO ()
main = play (InWindow "Scrabble" windowSize (0, 0)) white 1 initialWorld drawWorld handleEvent updateWorld

updateWorld :: Float -> World -> World
updateWorld _ = id

-- Controls

handleEvent :: Event -> World -> World
handleEvent (EventKey key Down _ _) w@(board, cursor) = case key of
  Char c -> handleUpdateLetter c w
  SpecialKey KeySpace -> handleUpdateLetter ' ' w
  SpecialKey k -> (board, handleUpdateCursor k cursor)
  _ -> w
handleEvent _ w = w

handleUpdateLetter :: Char -> World -> World
handleUpdateLetter c w@(board, cursor)
  | isValidInput c = (set board cursor (Character $ normalizeInput c), cursor)
  | c == ' ' = (set board cursor Empty, cursor)
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
