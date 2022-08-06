{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericRobot where

import Control.Applicative (Alternative (some))
import Control.Monad (when)
import Control.Monad.State (evalState, runState)
import Control.Monad.State.Class (MonadState (..))
import Data.Attoparsec.ByteString.Char8
  ( anyChar,
    char,
    decimal,
    endOfInput,
    parseOnly,
    skipSpace,
  )
import Data.ByteString (ByteString)
import Data.Kind (Type)

data Orientation = N | E | S | W
  deriving (Show, Eq, Enum, Bounded, Read)

data RobotState a = Located a | Lost a
  deriving (Show, Eq)

type SimpleGrid = (Int, Int)

class Monad m => ControlPlane m a where
  move :: [Movement a] -> m (RobotState a)

class Context a where
  type Grid a :: Type
  type Position a :: Type

-- BUSINESS LOGIC

class Robot a where
  type Movement a :: Type

  movementSize :: Int
  movements :: [Movement a]
  isInsideGrid :: a -> Grid a -> Bool
  nextMovement :: a -> Movement a -> a

newtype RobotT (m :: Type -> Type) (a :: Type) = RobotT {runRobotT :: m a}
  deriving (Show, Eq, Functor, Applicative, Monad, MonadState r)

type State a = (Grid a, RobotState a)

instance
  ( Robot a,
    MonadState (State a) m
  ) =>
  ControlPlane (RobotT m) a
  where
  move :: [Movement a] -> RobotT m (RobotState a)
  move [] = snd <$> get
  move (movement : remainingMovements) = do
    (grid, robotState) <- get
    case robotState of
      Lost robot -> pure $ Lost robot
      Located robot -> do
        let robot' = nextMovement robot movement
        put
          if robot' `isInsideGrid` grid
            then (grid, Located robot')
            else (grid, Lost robot)
        move remainingMovements

data SimpleMovement = F | L | R
  deriving (Show, Eq, Enum, Bounded, Read)

type SimplePosition = (Int, Int)

data SimpleRobot = SimpleRobot
  { srPosition :: SimplePosition,
    srOrientation :: Orientation
  }
  deriving (Show, Eq)

isInside :: (Num a, Ord a) => (a, a) -> (a, a) -> Bool
(x, y) `isInside` (m, n) =
  x >= 0 && x <= m && y >= 0 && y <= n

instance Context SimpleRobot where
  type Grid SimpleRobot = SimpleGrid
  type Position SimpleRobot = SimplePosition

instance Robot SimpleRobot where
  type Movement SimpleRobot = SimpleMovement

  movementSize = 1
  movements = [minBound ..]

  SimpleRobot {srPosition = (x, y)} `isInsideGrid` grid =
    (x, y) `isInside` grid

  nextMovement SimpleRobot {srPosition = (x, y), srOrientation} mvmt =
    uncurry
      SimpleRobot
      case (srOrientation, mvmt) of
        (N, L) -> ((x, y), W)
        (N, R) -> ((x, y), E)
        (N, F) -> ((x, y + 1), N)
        (E, L) -> ((x, y), N)
        (E, R) -> ((x, y), S)
        (E, F) -> ((x + 1, y), E)
        (S, L) -> ((x, y), E)
        (S, R) -> ((x, y), W)
        (S, F) -> ((x, y - 1), S)
        (W, L) -> ((x, y), S)
        (W, R) -> ((x, y), N)
        (W, F) -> ((x - 1, y), W)

-- PARSER

class RobotParser a where
  parseRobotAndMovements :: ByteString -> Either String (a, [Movement a])
  parseGrid :: ByteString -> Either String (Grid a)

instance RobotParser SimpleRobot where
  parseGrid = parseOnly grid
    where
      grid = do
        (n, m) <-
          (,)
            <$> (skipSpace *> decimal)
            <*> (skipSpace *> decimal <* skipSpace <* endOfInput)
        -- If they are less than 0 then either of these two might be the reason:
        -- 1. the input was negative
        -- 2. the input was too big and caused an overflow
        when (n <= 0 || m <= 0) do
          fail $
            "Grid bounds must be greater than 0 but were: ("
              ++ show n
              ++ ", "
              ++ show m
              ++ ")"
        pure (n, m)

  parseRobotAndMovements = parseOnly parser
    where
      parser = (,) <$> robot <*> (skipSpace *> moves <* endOfInput)

      orientation = do
        c <- anyChar
        if c `elem` show [minBound :: Orientation ..]
          then pure $ read @Orientation [c]
          else
            fail $
              "Illegal orientation '"
                ++ [c]
                ++ "'. Legal orientations are: "
                ++ show [minBound :: Orientation ..]

      robot = do
        let comma = skipSpace *> char ',' <* skipSpace
        (x, y, orientation') <-
          (,,)
            <$> (char '(' *> decimal)
            <*> (comma *> decimal)
            <*> (comma *> orientation <* char ')')
        -- If they are less than 0 then either of these two might be the reason:
        -- 1. the input was negative
        -- 2. the input was too big and caused an overflow
        when (x <= 0 || y <= 0) do
          fail $
            "Coordinates must be between 0 and " ++ show (maxBound :: Int)
              ++ " but were: ("
              ++ show x
              ++ ", "
              ++ show y
              ++ ")"
        pure
          SimpleRobot
            { srPosition = (x, y),
              srOrientation = orientation'
            }

      moves =
        some do
          c <- anyChar
          if c `elem` show (movements @SimpleRobot)
            then pure $ read @SimpleMovement [c]
            else
              fail $
                "Illegal move '" ++ [c] ++ "'. Legal moves are: "
                  ++ show [minBound :: SimpleMovement ..]

-- RUN

prettyRobot :: SimpleRobot -> String
prettyRobot SimpleRobot {..} =
  show (fst srPosition, snd srPosition, srOrientation)

prettyState :: RobotState SimpleRobot -> String
prettyState = \case
  Lost robot -> prettyRobot robot ++ " LOST"
  Located robot -> prettyRobot robot

type SimpleRobotState = State SimpleRobot

executeMission :: Robot a => [Movement a] -> (Grid a, RobotState a) -> RobotState a
executeMission movements' =
  evalState (runRobotT $ move movements')

runSimpleRobot :: (RobotState SimpleRobot, SimpleRobotState)
runSimpleRobot =
  runState
    -- runIdentity $ runStateT
    (runRobotT $ move [F, L, L, F, R])
    ((4, 8), Located $ SimpleRobot (2, 3) N)

-- >>> fst runSimpleRobot
-- >>> movements @SimpleRobot
-- >>> movementSize @SimpleRobot
-- >>> parseGrid @SimpleRobot "  4 8"
-- >>> parseRobotAndMovements @SimpleRobot "(4, 8, W) FFLLRRFLR"
-- Located (SimpleRobot {srPosition = (2,3), srOrientation = W})
-- [F,L,R]
-- 1
-- Right (4,8)
-- Right (SimpleRobot {srPosition = (4,8), srOrientation = W},[F,F,L,L,R,R,F,L,R])
