{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Robot
  ( Orientation (..),
    Movement (..),
    Robot (..),
    RobotState (..),
    Position,
    Grid,
    move,
    prettyState,
    checkGrid,
    checkPosition,
  )
where

data Orientation = N | E | S | W
  deriving (Show, Eq, Enum, Bounded)

data Movement = F | L | R
  deriving (Show, Eq, Enum, Bounded)

data Robot = Robot
  { rPosition :: Position,
    rOrientation :: Orientation
  }
  deriving (Show, Eq)

prettyRobot :: Robot -> String
prettyRobot Robot {..} =
  show (fst rPosition, snd rPosition, rOrientation)

{-
An alternative implementation could have been:

data Robot = Robot
  { rPosition :: Position,
    rOrientation :: Orientation,
    rState :: RobotState
  }
data RobotState = Located | Lost

There's not much difference in this small sample app.
The question to me boils down to: is this state a property
of the robot itself or something the controller care about
the robot? The current implementation leans towards the
latter.
-}

data RobotState = Located Robot | Lost Robot
  deriving (Show, Eq)

prettyState :: RobotState -> String
prettyState = \case
  Lost robot -> prettyRobot robot ++ " LOST"
  Located robot -> prettyRobot robot

-- >>> (0, maxBound::Int)
-- (0,9223372036854775807)
-- The grid cannot be greater than the maximum bound for a

checkBounds :: Num a => (a, a) -> (a -> Bool) -> Bool
checkBounds (x, y) f = f x || f y

type Position = (Int, Int)

checkPosition :: (Ord a, Num a) => (a, a) -> Bool
checkPosition = flip checkBounds (< 0)

type Grid = (Int, Int)

checkGrid :: (Ord a, Num a) => (a, a) -> Bool
checkGrid = flip checkBounds (<= 0)

-- TODO: add more unit tests to this function
move :: [Movement] -> Grid -> RobotState -> RobotState
move [] _ state = state
move _ _ lost@Lost {} = lost
move (movement : remainingMovements) grid robot' =
  let state' = updateRobotState movement grid robot'
   in move remainingMovements grid state'

-- TODO: add unit tests to this function
updateRobotState :: Movement -> Grid -> RobotState -> RobotState
updateRobotState movement grid = \case
  Lost robot -> Lost robot
  Located robot ->
    let robot' = nextMovement robot movement
     in if robot' `isInsideGrid` grid then Located robot' else Lost robot

isInsideGrid :: Robot -> Grid -> Bool
Robot {rPosition = (x, y)} `isInsideGrid` (m, n) =
  x >= 0 && x <= m && y >= 0 && y <= n

-- TODO: add unit tests to this function
nextMovement :: Robot -> Movement -> Robot
nextMovement = curry robot
  where
    robot (Robot {rPosition = (x, y), rOrientation}, movement) =
      case (rOrientation, movement) of
        (N, L) -> Robot (x, y) W
        (N, R) -> Robot (x, y) E
        (N, F) -> Robot (x, y + 1) N
        (E, L) -> Robot (x, y) N
        (E, R) -> Robot (x, y) S
        (E, F) -> Robot (x + 1, y) E
        (S, L) -> Robot (x, y) E
        (S, R) -> Robot (x, y) W
        (S, F) -> Robot (x, y - 1) S
        (W, L) -> Robot (x, y) S
        (W, R) -> Robot (x, y) N
        (W, F) -> Robot (x - 1, y) W
