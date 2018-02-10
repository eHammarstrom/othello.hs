module Test where

import Distribution.TestSuite
import Othello

debugPrintBoard''' :: IO ()
debugPrintBoard''' = putStrLn $ showBoard $ initialBoard 8

debugBoard =
  [ [Square (1, 1) (Just Black), Square (2, 1) Nothing]
  , [Square (1, 2) Nothing, Square (2, 2) (Just White)]
  ]

debugBoard' =
  [ [Square (1, 1) (Just White), Square (2, 1) (Just Black), Square (3, 1) Nothing]
  , [Square (1, 2) Nothing, Square (2, 2) (Just Black), Square (3, 2) (Just Black)]
  , [Square (1, 3) (Just White), Square (2, 3) (Just White), Square (3, 3) (Just White)]
  ]

debugValidMove = validMove (Move (0, 0) White) debugBoard
debugValidMove' = validMove (Move (3, 1) White) debugBoard'

debugState = State { turn = White, board = debugBoard}

-- Should not work, out side of board
debugEvent = updateState (Move' (0,0) White) debugState

-- Should work, flipping multiple rows and diagonals
debugEvent' = updateState (Move' (3,1) White) (debugState { board = debugBoard' })

-- Test: 'correct' move outside board
eventInvalidMove =
  let (state, err) = debugEvent in
    board state == board debugState
    &&
    err == MoveError

-- Test: flip multiple directions
eventFlipMultipleDirections =
  let (state, err) = debugEvent' in
    board state == [ [Square (1, 1) (Just White), Square (2, 1) (Just White), Square (3, 1) (Just White)]
                  , [Square (1, 2) Nothing, Square (2, 2) (Just White), Square (3, 2) (Just White)]
                  , [Square (1, 3) (Just White), Square (2, 3) (Just White), Square (3, 3) (Just White)]
                  ]
    &&
    err == Nil

-- run tests
tests :: IO [Test]
tests = return [ Test invalidMove, Test flipMultiple ]
  where
    flipMultiple = TestInstance
      { run = return $ Finished $ if eventFlipMultipleDirections then Pass else Fail "failed"
      , name = "flip multiple directions"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right flipMultiple
      }
    invalidMove = TestInstance
      { run = return $ Finished $ if eventInvalidMove then Pass else Fail "failed"
      , name = "'correct' move outside board"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right invalidMove
      }
