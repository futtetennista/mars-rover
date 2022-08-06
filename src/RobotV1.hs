{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module RobotV1
  ( Orientation (..),
    Move (..),
    Robot (..),
    RobotState (..),
    Position,
    Grid,
    moveRobot,
    prettyState,
  )
where

data Orientation = N | E | S | W
  deriving (Show, Eq, Enum, Bounded)

data Move = F | L | R
  deriving (Show, Eq, Enum, Bounded)

data RobotState = Found Robot | Lost Robot
  deriving (Show, Eq)

prettyState :: RobotState -> String
prettyState = \case
  Lost robot -> prettyRobot robot ++ " LOST"
  Found robot -> prettyRobot robot

type Position = (Int, Int)

type Grid = (Int, Int)

moveRobot :: [Move] -> Grid -> RobotState -> RobotState
moveRobot [] _ state = state
moveRobot _ _ lost@Lost {} = lost
moveRobot (move : remainingMoves) grid robot' =
  let state' = updateRobotState move grid robot'
   in moveRobot remainingMoves grid state'

updateRobotState :: Move -> Grid -> RobotState -> RobotState
updateRobotState move (m, n) = \case
  Lost robot -> Lost robot
  Found robot ->
    let robot' = applyNextMove robot move
     in if isInsideGrid robot' then Found robot' else Lost robot
  where
    isInsideGrid Robot {rPosition = (x, y)} =
      x >= 0 && x <= m && y >= 0 && y <= n

applyNextMove :: Robot -> Move -> Robot
applyNextMove = curry robot
  where
    robot (Robot {rPosition = (x, y), rOrientation}, move) =
      case (rOrientation, move) of
        (N, L) -> Robot (x, y) W
        (N, R) -> Robot (x, y) E
        (N, F) -> Robot (x, y + 1) N
        (E, L) -> Robot (x, y) N
        (E, R) -> Robot (x, y) S
        (E, F) -> Robot (x + 1, y) E
        (S, L) -> Robot (x, y) W
        (S, R) -> Robot (x, y) E
        (S, F) -> Robot (x, y - 1) S
        (W, L) -> Robot (x, y) S
        (W, R) -> Robot (x, y) N
        (W, F) -> Robot (x - 1, y) W

data Robot = Robot
  { rPosition :: Position,
    rOrientation :: Orientation
  }
  deriving (Show, Eq)

prettyRobot :: Robot -> String
prettyRobot Robot {..} =
  show (fst rPosition, snd rPosition, rOrientation)

-- class RobotOperations a where
--   next :: Move -> a -> a
--   step :: a -> Int
--   move :: [Move] -> GenericRobotState a

-- move [] st = st
-- move _ lost@Lost{} = lost
-- move (m:ms) (Found robot) = do
--   (grid, robot) <- get
--   case st of
--     Lost robot -> Lost robot
--     Found robot ->
--       pure $ next move robot
