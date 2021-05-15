module WordSearch where

import BoardData

import qualified Data.Set as S
import Data.List

type Hand = [Char]
type WordDirection = Down | Right
type WordPlacement = (String, MatrixIndex, WordDirection)
type Dictionary = S.Set String

ortogonal :: WordDirection -> WordDirection
ortogonal Down = Right
ortogonal Right = Down

move :: MatrixIndex -> WordDirection -> MatrixIndex
move (col, row) Down = (col, row + 1)
move (col, row) Right = (col + 1, row)

search :: Board -> Hand -> Dictionary -> [WordPlacement]
search board hand dict = do
  -- the topmost, leftmost position of the placed word
  firstCol <- [0 .. boardWidth - 1]
  firstRow <- [0 .. boardHeight - 1]
  direction <- [Down, Right]
  let prefix = getPrefix (firstCol, firstRow) direction board
  searchFrom (firstCol, firstRow) direction board hand dict

searchFrom :: MatrixIndex -> Direction -> String -> Board -> [Char] -> Dictionary -> [WordPlacement]
searchFrom startIdx dir startPrefix board startHand dict = f startIdx stardPrefix startHand
  where
    f idx _ _ | (board `at` idx) != Empty = []
    f idx prefix hand =
      let continue c = f (move idx dir) (prefix ++ [c]) (delete c hand)
      map (\c -> if (prefix ++ [c]) `S.member` dict
                 then ((prefix ++ [c]), startIdx, dir) : continue c
                 else continue c
      filter (\c -> checkOrtogonalDirection idx dir bord dict) hand

checkOrtogonalDirection :: MatrixIndex -> Direction -> Board -> Dictionary -> Bool
checkOrtogonalDirection idx dir board dict = length w == 1 || w `S.member` dict
  where w = getFullWord idx (ortogonal direction) board

getFullWord :: MatrixIndex -> Char -> Direction -> Board -> String
getFullWord pos c dir board = getPrefix pos dir board ++ c : getSuffix pos dir board

getPrefix :: MatrixIndex -> Direction -> Board -> String
getPrefix (col, row) dir board = reverse $ takeNonempty prefPositions board
  where
    prefPositions = case dir of
      Down -> map (col,) [row - 1, row - 2 .. 0]
      Right -> map (,row) [col - 1, col - 2 .. 0]

getSuffix :: MatrixIndex -> Direction -> Board -> String
getSuffix (col, row) dir board = takeNonempty prefPositions board
  where
    prefPositions = case dir of
      Down -> map (col,) [row + 1, row + 2 .. boardHeight - 1]
      Right -> map (,row) [col + 1, col + 2 .. boardWidth - 1]

takeNonempty :: [MatrixIndex] -> Board -> String
takeNonempty positions board =
  map (\(Character c) -> c) $
  takeWhile (\pos -> (board `at` pos) != Empty) positions
