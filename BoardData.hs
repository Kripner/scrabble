module BoardData where

import Data.Char
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Utils

type Hand = [TileContent]

handSize = 7 :: Int

initialHand = replicate handSize Empty

type Dictionary = Set.Set String

data Cursor = BoardCursor MatrixIndex | HandIndex Int

data World = World
  { board :: Board,
    cursor :: Cursor,
    hand :: Hand,
    dictionary :: Dictionary
  }

initialWorld :: Dictionary -> World
initialWorld dictionary =
  World
    { board = initialBoard,
      cursor = BoardCursor (boardWidth // 2, boardHeight // 2),
      hand = initialHand,
      dictionary = dictionary
    }

cycleCursor :: Cursor -> Cursor
cycleCursor (BoardCursor _) = HandIndex 0
cycleCursor (HandIndex _) = BoardCursor (boardWidth // 2, boardHeight // 2)

data TileType = Normal | DoubleLetter | TripleLetter | DoubleWord | TripleWord | CenterTile
  deriving (Eq, Show)

data TileContent = Empty | Character Char
  deriving (Eq, Show)

isValidInput :: Char -> Bool
isValidInput c = isAsciiLower c || isAsciiUpper c

normalizeInput :: Char -> Char
normalizeInput = toUpper

type Matrix a = S.Seq (S.Seq a)

type Tiles = Matrix TileType

type Board = Matrix TileContent

tiles :: Tiles
tiles = foldr (\row tiles' -> (S.fromList row) S.<| tiles') S.empty tilesArray

boardHeight = length tilesArray

boardWidth = length (head tilesArray)

initialBoard :: Board
initialBoard = S.replicate boardHeight (S.replicate boardWidth Empty)

type MatrixIndex = (Int, Int)

at :: Matrix a -> MatrixIndex -> a
at mat = uncurry (S.index . S.index mat)

set :: Matrix a -> MatrixIndex -> a -> Matrix a
set mat (col, row) val = S.update col (S.update row val (S.index mat col)) mat

isInside :: MatrixIndex -> Bool
isInside (col, row) = col >= 0 && col < boardWidth && row >= 0 && row < boardHeight

tilesArray :: [[TileType]]
tilesArray =
  [ [TripleWord, Normal, Normal, DoubleLetter, Normal, Normal, Normal, TripleWord, Normal, Normal, Normal, DoubleLetter, Normal, Normal, TripleWord],
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
    [TripleWord, Normal, Normal, DoubleLetter, Normal, Normal, Normal, TripleWord, Normal, Normal, Normal, DoubleLetter, Normal, Normal, TripleWord]
  ]
