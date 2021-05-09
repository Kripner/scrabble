module BoardData where

import qualified Data.Sequence as S

data TileType = Normal | DoubleLetter | TrippleLetter | DoubleWord | TrippleWord
data TileContent = Empty | Character Char

type Matrix a = S.Seq (S.Seq a)
type Tiles = Matrix TileType
type Board = Matrix TileContent

tiles :: Tiles
tiles = foldr (\row tiles' -> (S.fromList row) S.<| tiles') S.empty tilesArray

boardHeight = length tilesArray
boardWidth = length (head tilesArray)

initialBoard :: Board
initialBoard = S.replicate boardHeight (S.replicate boardWidth (Character 'A'))

type MatrixIndex = (Int, Int)

at :: Matrix a -> MatrixIndex -> a
at mat (col, row) = (S.index . S.index mat) col row
-- TODO: maybe use curry?

tilesArray :: [[TileType]]
tilesArray = [
  [Normal, DoubleLetter, TrippleLetter],
  [DoubleWord, TrippleWord, Normal],
  [Normal, Normal, Normal]]
