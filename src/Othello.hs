module Othello where

import Data.List
import Data.Maybe
import Debug.Trace

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

data Error = MoveError | TurnError | Nil deriving (Eq, Show)

initialBoard :: Integer -> Board
initialBoard n = [ [ Square (x, y) Nothing | x <- [1 .. n] ] | y <- [1 .. n] ]

showBoard :: Board -> String
showBoard b = intercalate "\n" (map show b)

updateState :: Event -> State -> (State, Error)
updateState e s = case e of
  (Turn Black) ->
    if turn s == White then (s { turn = Black }, Nil) else (s, TurnError)
  (Turn White) ->
    if turn s == Black then (s { turn = White }, Nil) else (s, TurnError)
  (Move' l p)  -> if validMove (Move l p) (board s)
    then (s { board = board' }, Nil)
    else (s, MoveError)
    where board' = updateBoard (Move l p) (board s)

searchDirections :: [Direction]
searchDirections =
  filter (/= (0, 0)) [ (x, y) | x <- [-1 .. 1], y <- [-1 .. 1] ]

validMove :: Move -> Board -> Bool
validMove move board =
  let
    Move location player = move
    searchCutoff         = fromIntegral $ length board
    paths                = map (walkAndCollect searchCutoff location board) searchDirections
    validPaths           = map (validMovePath player) paths
  in
    freePosition location board && or validPaths

{- Unused, implicitly being checked in freePosition
onBoard :: Location -> Board -> Bool
onBoard (x, y) board = x >= 1 && x <= bound && y >= 1 && y <= bound
    where
      bound = fromIntegral $ length board
-}

freePosition :: Location -> Board -> Bool
freePosition l b = case getSquareByLocation l b of
  Just (Square _ Nothing) -> True
  _                       -> False

negPlayer :: Player -> Player
negPlayer Black = White
negPlayer White = Black

-- This provides the actual valid move definition (2 pieces surrounding only enemy pieces)
validMovePath :: Player -> [Square] -> Bool
validMovePath p []  = False
validMovePath p [_] = False
validMovePath p ss  =
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
  let flatBoard       = concat board
      (x, y)          = dir
      locationsToWalk = [ addLocation loc (scaleLocation n dir) | n <- [1 .. cutoff] ]
      squares         = map (`getSquareByLocation` board) locationsToWalk
  in  catMaybes squares

updateBoard :: Move -> Board -> Board
updateBoard move board =
  let
    Move location player = move
    searchCutoff         = fromIntegral $ length board
    paths                = map (walkAndCollect searchCutoff location board) searchDirections
    squaresToPlace       = concatMap (\p -> if validMovePath player p then p else []) paths
    squaresToPlace'      = Square location (Just player) : squaresToPlace -- Include the piece being placed
    squares              = map (setColor player) squaresToPlace'
  in
    placeSquares squares board

placeSquares :: [Square] -> Board -> Board
placeSquares ss =
  (map . map) (replaceSquare ss)
    where
      replaceSquare [] s = s
      replaceSquare (Square loc' p' : ss) (Square loc p) =
        if loc == loc'
          then Square loc' p'
          else replaceSquare ss (Square loc p)


setColor :: Player -> Square -> Square
setColor player (Square pos _) = Square pos (Just player)

getSquareByLocation :: Location -> Board -> Maybe Square
getSquareByLocation l board = find findSquareOnLocation (concat board)
  where findSquareOnLocation (Square l' _) = l == l'
