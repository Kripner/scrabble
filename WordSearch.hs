module WordSearch where

import BoardData
import Data.List
import qualified Data.Set as S
import Prelude hiding (Right)

type Hand = [Char]

data Direction = Down | Right

type WordPlacement = (String, MatrixIndex, Direction)

type Dictionary = S.Set String

orthogonal :: Direction -> Direction
orthogonal Down = Right
orthogonal Right = Down

move :: MatrixIndex -> Direction -> MatrixIndex
move (col, row) Down = (col, row + 1)
move (col, row) Right = (col + 1, row)

hasNeighbour :: MatrixIndex -> Board -> Bool
hasNeighbour (col, row) board = any (\idx -> isInside idx && board `at` idx != Empty) neighbours
  where
    neighbours = [(col + 1, row), (col - 1, row), (col, row + 1), (col, row - 1)]

search :: Board -> Hand -> Dictionary -> [WordPlacement]
search board hand dict = do
  -- the topmost, leftmost position of the placed word
  firstCol <- [0 .. boardWidth - 1]
  firstRow <- [0 .. boardHeight - 1]
  direction <- [Down, Right]
  let prefix = getPrefix (firstCol, firstRow) direction board
  searchFrom (firstCol, firstRow) direction board hand dict

searchFrom :: MatrixIndex -> Direction -> String -> Board -> [Char] -> Dictionary -> [WordPlacement]
searchFrom startIdx dir startPrefix board startHand dict = f descriptors startPrefix startHand
  where
    descriptors = getDescriptors startIdx dir board (length startHand)
    f [] _ _ = []
    f ((Descriptor idx anchored oPref oSuf suffix) : descriptors) prefix hand
      | not ((null oPref && null oSuf) || (oPref ++ c : oSuf) `S.member` dict) = []
      | otherwise =
          let
            newPref c = prefix ++ [c]
            fullWord c = prefix ++ c : suffix
            validWord c = anchored && (fullWord c) `S.member` dict
            continue c = f descriptors (newPref c) (delete c hand)
           in map (\c -> if validWord c then (newPref c, startIdx, dir) : continue c else continue c

getDescriptors :: MatrixIndex -> Direction -> Board -> Int -> [PositionDescriptor]
getDescriptors startIdx dir board maxLen =
  let indexes = take maxLen $ takeWhile (\pos -> (board `at` pos) == Empty) $ walk startIdx dir
      hadNeighbour = foldl (\had idx -> had || hasNeighbour idx board) False allIndexes
      orthogonalDir = orthogonal dir
   in map (\idx anchored -> PositionDescriptor
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

--checkOrtogonalDirection :: MatrixIndex -> Direction -> Board -> Dictionary -> Bool
--checkOrtogonalDirection idx dir board dict = length w == 1 || w `S.member` dict
--  where w = getFullWord idx (orthogonal direction) board
--
--getFullWord :: MatrixIndex -> Char -> Direction -> Board -> String
--getFullWord pos c dir board = getPrefix pos dir board ++ c : getSuffix pos dir board

getPrefix :: MatrixIndex -> Direction -> Board -> String
getPrefix (col, row) dir = reverse $ takeNonempty prefPositions
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
  Down -> map (col,) [row, row + 1 .. boardHeight - 1]
  Right -> map (,row) [col, col + 1 .. boardWidth - 1]

takeNonempty :: [MatrixIndex] -> Board -> String
takeNonempty positions board =
  map (\(Character c) -> c) $
    takeWhile (\pos -> (board `at` pos) != Empty) positions
