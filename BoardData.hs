module BoardData where

import qualified Data.Sequence as S

data TileType = Normal | DoubleLetter | TripleLetter | DoubleWord | TripleWord | CenterTile
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
  [TripleWord, Normal, Normal, DoubleLetter, Normal, Normal, Normal, TripleWord, Normal, Normal, Normal, DoubleLetter, Normal, Normal, TripleWord],
  [Normal, DoubleWord, Normal, Normal, Normal, TripleLetter, Normal, Normal, Normal, TripleLetter, Normal, Normal, Normal, DoubleWord, Normal],
  [Normal, Normal, DoubleWord, Normal, Normal, Normal, DoubleLetter, Normal, DoubleLetter, Normal, Normal, Normal, DoubleWord, Normal, Normal],
  [DoubleLetter, Normal, Normal, DoubleWord, Normal, Normal, Normal, DoubleWord, Normal, Normal, Normal, DoubleWord, Normal, Normal, DoubleLetter],
  [Normal, Normal, Normal, Normal, DoubleWord, Normal, Normal, Normal, Normal, Normal, DoubleWord, Normal, Normal, Normal, Normal],
  [Normal, TripleLetter, Normal, Normal, Normal, TripleLetter, Normal, Normal, Normal, TripleLetter, Normal, Normal, Normal, TripleLetter, Normal],
  [Normal, Normal, DoubleLetter, Normal, Normal, Normal, DoubleLetter, Normal, DoubleLetter, Normal, Normal, Normal, DoubleLetter, Normal, Normal],

  [TripleWord, Normal, Normal, DoubleLetter, Normal, Normal, Normal, CenterTile, Normal, Normal, Normal, DoubleLetter, Normal, Normal, TripleWord],

  [Normal, Normal, DoubleLetter, Normal, Normal, Normal, DoubleLetter, Normal, DoubleLetter, Normal, Normal, Normal, DoubleLetter, Normal, Normal],
  [Normal, TripleLetter, Normal, Normal, Normal, TripleLetter, Normal, Normal, Normal, TripleLetter, Normal, Normal, Normal, TripleLetter, Normal],
  [Normal, Normal, Normal, Normal, DoubleWord, Normal, Normal, Normal, Normal, Normal, DoubleWord, Normal, Normal, Normal, Normal],
  [DoubleLetter, Normal, Normal, DoubleWord, Normal, Normal, Normal, DoubleWord, Normal, Normal, Normal, DoubleWord, Normal, Normal, DoubleLetter],
  [Normal, Normal, DoubleWord, Normal, Normal, Normal, DoubleLetter, Normal, DoubleLetter, Normal, Normal, Normal, DoubleWord, Normal, Normal],
  [Normal, DoubleWord, Normal, Normal, Normal, TripleLetter, Normal, Normal, Normal, TripleLetter, Normal, Normal, Normal, DoubleWord, Normal],
  [TripleWord, Normal, Normal, DoubleLetter, Normal, Normal, Normal, TripleWord, Normal, Normal, Normal, DoubleLetter, Normal, Normal, TripleWord]]
