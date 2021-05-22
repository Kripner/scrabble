{-# LANGUAGE TupleSections #-}

module WordSearch where

import BoardData
import Utils

import Data.Char
import Data.List
import qualified Data.Set as S
import Prelude hiding (Right)
import Debug.Trace

data Direction = Down | Right
  deriving (Eq, Show)

type WordPlacement = (String, MatrixIndex, Direction)

loadDictionary :: String -> IO Dictionary
loadDictionary file = S.fromList <$> (map . map) toUpper <$> lines <$> readFile file

sampleHand = "HELL"
sampleDict = S.fromList ["HELLO", "BOX", "WINDOW"]


orthogonal :: Direction -> Direction
orthogonal Down = Right
orthogonal Right = Down

move :: MatrixIndex -> Direction -> MatrixIndex
move (col, row) Down = (col, row + 1)
move (col, row) Right = (col + 1, row)

hasNeighbour :: MatrixIndex -> Board -> Bool
hasNeighbour (col, row) board = any (\idx -> isInside idx && board `at` idx /= Empty) neighbours
  where
    neighbours = [(col + 1, row), (col - 1, row), (col, row + 1), (col, row - 1)]

search :: Board -> Hand -> Dictionary -> [WordPlacement]
search board hand dict = concat $ do
  -- the topmost, leftmost position of the placed word
  let availChars = [c | Character c <- hand]
  firstCol <- [0 .. boardWidth - 1]
  firstRow <- [0 .. boardHeight - 1]
  direction <- [Down, Right]
  let prefix = getPrefix (firstCol, firstRow) direction board
  return $ searchFrom (firstCol, firstRow) direction prefix board availChars dict

-- TODO: use tries
searchFrom :: MatrixIndex -> Direction -> String -> Board -> [Char] -> Dictionary -> [WordPlacement]
searchFrom startIdx dir startPrefix board startHand dict = searchFrom' descriptors startPrefix startHand
  where
    descriptors = getDescriptors startIdx dir board (length startHand)
    searchFrom' [] _ _ = []
    searchFrom' ((PositionDescriptor idx anchored oPref oSuf suffix) : descriptors) prefix hand =
      let
        newPref c = prefix ++ [c]
        fullWord c = prefix ++ c : suffix
        validWord c = anchored && (fullWord c) `S.member` dict
        continue c = searchFrom' descriptors (newPref c) (delete c hand)
       in concat $
          map (\c -> if validWord c then (fullWord c, startIdx, dir) : continue c else continue c) $
          filter (\c -> (null oPref && null oSuf) || (oPref ++ c : oSuf) `S.member` dict) hand

getDescriptors :: MatrixIndex -> Direction -> Board -> Int -> [PositionDescriptor]
getDescriptors startIdx dir board maxLen =
  let indexes = take maxLen $ takeWhile (\pos -> (board `at` pos) == Empty) $ walk startIdx dir
      hadNeighbour = tail $ scanl (\had idx -> had || hasNeighbour idx board) False indexes
      orthogonalDir = orthogonal dir
   in map (\(idx, anchored) -> PositionDescriptor
        idx
        anchored
        (getPrefix idx orthogonalDir board)
        (getSuffix idx orthogonalDir board)
        (getSuffix idx dir board)
      ) (zip indexes hadNeighbour)

data PositionDescriptor = PositionDescriptor
  { idx :: MatrixIndex,
    isAnchored :: Bool,
    orthogonalPref :: String,
    orthogonalSuff :: String,
    suffix :: String
  }
  deriving (Eq, Show)

--checkOrtogonalDirection :: MatrixIndex -> Direction -> Board -> Dictionary -> Bool
--checkOrtogonalDirection idx dir board dict = length w == 1 || w `S.member` dict
--  where w = getFullWord idx (orthogonal direction) board
--
--getFullWord :: MatrixIndex -> Char -> Direction -> Board -> String
--getFullWord pos c dir board = getPrefix pos dir board ++ c : getSuffix pos dir board

getPrefix :: MatrixIndex -> Direction -> Board -> String
getPrefix (col, row) dir board = reverse $ takeNonempty prefPositions board
  where
    prefPositions = case dir of
      Down -> walkRev (col, row - 1) Down
      Right -> walkRev (col - 1, row) Right

getSuffix :: MatrixIndex -> Direction -> Board -> String
getSuffix (col, row) dir = takeNonempty prefPositions
  where
    prefPositions = case dir of
      Down -> walk (col, row + 1) Down
      Right -> walk (col + 1, row) Right

walk :: MatrixIndex -> Direction -> [MatrixIndex]
walk (col, row) dir = case dir of
  Down -> map (col,) [row, row + 1 .. boardHeight - 1]
  Right -> map (,row) [col, col + 1 .. boardWidth - 1]

walkRev :: MatrixIndex -> Direction -> [MatrixIndex]
walkRev (col, row) dir = case dir of
  Down -> map (col,) [row, row - 1 .. boardHeight - 1]
  Right -> map (,row) [col, col - 1 .. boardWidth - 1]

takeNonempty :: [MatrixIndex] -> Board -> String
takeNonempty positions board =
  map (\(Character c) -> c) $ takeWhile (/= Empty) $ map (at board) positions

searchFiltered :: Board -> Hand -> Dictionary -> [WordPlacement]
searchFiltered board hand dict =
  fst $ foldl tryAdd ([], S.empty) allWords
  where
    allWords = search board hand dict
    tryAdd (results, seenWords) wp@(w, _, _) =
      if w `S.member` seenWords
          then (results, seenWords)
          else (wp : results, w `S.insert` seenWords)

searchSorted :: Board -> Hand -> Dictionary -> [WordPlacement]
searchSorted board hand dict = reverse $ sortOn (\(w, _, _) -> length w) words
  where words = searchFiltered board hand dict
