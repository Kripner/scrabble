module Main where

import BoardData
import qualified Data.Sequence as S
import Graphics
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Options.Applicative as Opt
import System.Exit
import Utils
import WordSearch hiding (Down)

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
  playIO (InWindow "Scrabble" windowSize (0, 0)) white 1 (initialWorld dictionary) drawWorld handleEvent updateWorld

updateWorld :: Float -> World -> IO World
updateWorld _ = return

printWords :: [WordPlacement] -> IO ()
printWords words = do
  putStr "\ESC[2J" -- clear the console
  if null words then putStrLn "No words found." else printWords'
  where
    printWords' = mapM_ (putStrLn . showWord) (reverse words)
    showWord (w, pos, dir) = w ++ " at " ++ showPosition pos ++ " (direction: " ++ show dir ++ ")"
    showPosition (col, row) = show (col + 1) ++ [toEnum (row + fromEnum 'A')]

-- Controls

handleEvent :: Event -> World -> IO World
handleEvent (EventKey key Down _ _) w@(World board cursor hand dict) = case key of
  Char c -> return $ handleUpdateLetter c w
  SpecialKey KeyEsc -> exitSuccess
  SpecialKey KeySpace -> return $ handleUpdateLetter ' ' w
  SpecialKey KeyTab -> return $ World board (cycleCursor cursor) hand dict
  SpecialKey KeyEnter -> printWords (take 50 (searchSorted board hand dict)) >> return w
  SpecialKey k -> return $ World board (handleUpdateCursor k cursor) hand dict
  _ -> return w
handleEvent _ w = return w

handleUpdateLetter :: Char -> World -> World
handleUpdateLetter c w@(World board cursor hand dict)
  | isValidInput c = updateTile cursor (Character $ normalizeInput c)
  | c == ' ' = updateTile cursor Empty
  | otherwise = w
  where
    updateTile (BoardCursor boardPos) newContent =
      World (set board boardPos newContent) cursor hand dict
    updateTile (HandIndex idx) newContent =
      World board cursor (take idx hand ++ newContent : drop (idx + 1) hand) dict

handleUpdateCursor :: SpecialKey -> Cursor -> Cursor
handleUpdateCursor key (BoardCursor (col, row)) =
  BoardCursor (clamp 0 (boardWidth - 1) newCol, clamp 0 (boardHeight - 1) newRow)
  where
    (newCol, newRow) = case key of
      KeyLeft -> (col - 1, row)
      KeyUp -> (col, row - 1)
      KeyRight -> (col + 1, row)
      KeyDown -> (col, row + 1)
      _ -> (col, row)
handleUpdateCursor key (HandIndex idx) =
  HandIndex $ clamp 0 (handSize - 1) newIdx
  where
    newIdx = case key of
      KeyLeft -> idx - 1
      KeyRight -> idx + 1
      _ -> idx
