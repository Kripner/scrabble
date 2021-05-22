module Main where

import BoardData
import WordSearch hiding (Down)
import Graphics
import Utils

import qualified Options.Applicative as Opt
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import qualified Data.Sequence as S

dictOption :: Opt.Parser String
dictOption =
  Opt.strOption $
    Opt.long "words"
      <> Opt.short 'w'
      <> Opt.value "words.txt"
      <> Opt.metavar "FILE"
      <> Opt.help "File containing all valid words, each on a separate line."

optsParser =
  Opt.info (Opt.helper <*> dictOption) $
    Opt.progDesc "Allows to manage content of a Scrabble board and search for words to place."
      <> Opt.header "Scrabble solver"
      <> Opt.footer "A project for NPRG005."

main :: IO ()
main = do
  dictFile <- Opt.execParser optsParser
  dictionary <- loadDictionary dictFile
  play (InWindow "Scrabble" windowSize (0, 0)) white 1 initialWorld drawWorld handleEvent updateWorld

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
