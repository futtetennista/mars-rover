module Parser
  ( parseGrid,
    parseRobot,
    parseMoves,
    parseRobotAndMoves,
  )
where

import Control.Applicative (some)
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    anyChar,
    char,
    decimal,
    endOfInput,
    parseOnly,
    skipSpace,
    space,
  )
import Data.ByteString (ByteString)
import RobotV1 (Grid, Move (..), Orientation (..), Robot (..))

-- | Parse a grid that must have bounds greater than 0.
--   e.g. 0 3 is an invalid grid while 10 3 is a valid grid.
parseGrid :: ByteString -> Either String Grid
parseGrid = parseOnly grid

grid :: Parser Grid
grid = do
  n <- skipSpace *> decimal
  m <- skipSpace *> decimal <* skipSpace <* endOfInput
  when (n <= 0 || m <= 0) do
    fail $
      "Grid bounds must be greater than 0 but were: ("
        ++ show n
        ++ ", "
        ++ show m
        ++ ")"
  pure (n, m)

-- >>> parseGrid "0  3"
-- >>> parseGrid "2  3"
-- >>> parseGrid "9223372036854775808 10"
-- Left "Failed reading: Grid bounds must be greater than 0 but were: (0, 3)"
-- Right (2,3)
-- Left "Failed reading: Grid bounds must be greater than 0 but were: (-9223372036854775808, 10)"

-- >>> parseRobotAndMoves "(1, 3, W) LFRL"
-- Right (Robot {rPosition = (1,3), rOrientation = W},[L,F,R,L])

parseRobotAndMoves :: ByteString -> Either String (Robot, [Move])
parseRobotAndMoves = parseOnly parser
  where
    parser = (,) <$> robot <*> (some space *> moves <* endOfInput)

parseMoves :: ByteString -> Either String [Move]
parseMoves = parseOnly moves

-- >>> parseOnly moves ""
-- >>> parseOnly moves "LRF"
-- Left "not enough input"
-- Right [L,R,F]

-- | Parse 1 or more moves and fails if no moves are provided
moves :: Parser [Move]
moves =
  some $
    anyChar >>= \case
      'F' -> pure F
      'L' -> pure L
      'R' -> pure R
      c ->
        fail $
          "Illegal move '" ++ [c] ++ "'. Legal moves are: "
            ++ show [minBound :: Move ..]

-- >>> parseRobot "(10, 20, N)"
-- >>> parseRobot "(10, 20, X)"
-- >>> parseRobot "(9223372036854775808, 10, W)"
-- Right (Robot {rPosition = (10,20), rOrientation = N})
-- Left "Failed reading: Illegal orientation 'X'. Legal orientations are: [N,E,S,W]"
-- Left "Failed reading: Coordinates must be between 0 and 9223372036854775807 but were: (-9223372036854775808, 10)"

parseRobot :: ByteString -> Either String Robot
parseRobot = parseOnly robot

robot :: Parser Robot
robot = do
  _ <- char '('
  x <- decimal
  _ <- comma
  y <- decimal
  _ <- comma
  orientation' <- orientation
  _ <- char ')'
  when (x <= 0 || x <= 0) do
    fail $
      "Coordinates must be between 0 and " ++ show (maxBound :: Int)
        ++ " but were: ("
        ++ show x
        ++ ", "
        ++ show y
        ++ ")"
  pure
    Robot
      { rPosition = (x, y),
        rOrientation = orientation'
      }
  where
    comma = skipSpace *> char ',' <* skipSpace

-- >>> parseRobot "(1, 2, W)"
-- >>> parseRobot "(1, 2, X)"
-- Right (Robot {rPosition = (1,2), rOrientation = W})
-- Left "Failed reading: Illegal orientation 'X'. Legal orientations are: [N,E,S,W]"

orientation :: Parser Orientation
orientation =
  anyChar >>= \case
    'N' -> pure N
    'E' -> pure E
    'S' -> pure S
    'W' -> pure W
    c ->
      fail $
        "Illegal orientation '" ++ [c]
          ++ "'. Legal orientations are: "
          ++ show [minBound :: Orientation ..]
