module Othello where

import Data.List
import Data.Maybe
-- import Debug.Trace

othello = "Fun, fun, fun!"

type Row = Integer
type Column = Integer
type Location = (Row, Column)
type Direction = (Integer, Integer)

data Player = Black | White deriving (Eq, Show)

data Square = Square Location (Maybe Player) deriving (Eq, Show)

data Move = Move Location Player deriving (Eq, Show)

type Board = [[Square]]

data State = State { board :: [[Square]]
                   , turn :: Player } deriving (Eq, Show)

data Event = Move' Location Player | Turn Player deriving (Eq, Show)

data Error = MoveError | TurnError | Nil deriving (Show)

initialBoard :: Integer -> Board
initialBoard n = [ [ Square (x, y) Nothing | x <- [1 .. n] ] | y <- [1 .. n] ]

showBoard :: Board -> String
showBoard b = intercalate "\n" (map show b)

debugPrintBoard''' :: IO ()
debugPrintBoard''' = putStrLn $ showBoard $ initialBoard 8

debugBoard =
  [ [(Square (1, 1) (Just Black)), (Square (2, 1) Nothing)]
  , [(Square (1, 2) Nothing), (Square (2, 2) (Just White))]
  ]

debugValidMove = validMove (Move (0, 0) White) debugBoard

updateState :: Event -> State -> (State, Error)
updateState e s = case e of
  (Turn Black) ->
    if turn s == White then (s { turn = Black }, Nil) else (s, TurnError)
  (Turn White) ->
    if turn s == Black then (s { turn = White }, Nil) else (s, TurnError)
  (Move' l p) -> if validMove (Move l p) (board s)
    then (s { board = board' }, Nil)
    else (s, MoveError)
    where board' = updateBoard (Move l p) (board s)

searchDirections :: [Direction]
searchDirections =
  filter (/= (0, 0)) [ (x, y) | x <- [-1 .. 1], y <- [-1 .. 1] ]

validMove :: Move -> Board -> Bool
validMove move board =
  let
    (Move location player) = move
    searchCutoff           = fromIntegral $ length board
    paths = map (walkAndCollect searchCutoff location board) searchDirections
    validPaths             = map (validMovePath player) paths
  in
    and validPaths

negPlayer :: Player -> Player
negPlayer Black = White
negPlayer White = Black

-- This provides the actual valid move definition (2 pieces surrounding only enemy pieces)
validMovePath :: Player -> [Square] -> Bool
validMovePath p [] = False
validMovePath p ss =
  let negPlayerSquare        = Square (0, 0) (Just (negPlayer p))
      xs                     = reverse ss
      enemiesBetween         = map (squarePlayerEq negPlayerSquare) (tail xs) -- tail is inbetween
      -- head is last square, should be same color,
      -- extract player from head for comparison
      (Square _ maybePlayer) = head xs
  in  case maybePlayer of
        Just p' -> (p == p') && and enemiesBetween
        Nothing -> False

squarePlayerEq :: Square -> Square -> Bool
squarePlayerEq (Square _ p) (Square _ p') = p == p'

addLocation :: Location -> Location -> Location
addLocation (x, y) (z, w) = (x + z, y + w)

scaleLocation :: Integer -> Location -> Location
scaleLocation n (x, y) = (x * n, y * n)

walkAndCollect :: Integer -> Location -> Board -> Direction -> [Square]
walkAndCollect cutoff loc board dir =
  let flatBoard = concat board
      (x, y)    = dir
      locationsToWalk =
        [ addLocation loc (scaleLocation n dir) | n <- [1 .. cutoff] ]
      squares = map (`getSquareByLocation` board) locationsToWalk
  in  catMaybes squares

updateBoard :: Move -> Board -> Board
updateBoard m b = undefined

getSquareByLocation :: Location -> Board -> Maybe Square
getSquareByLocation l board = find findSquareOnLocation (concat board)
  where findSquareOnLocation (Square l' _) = l == l'





















































































