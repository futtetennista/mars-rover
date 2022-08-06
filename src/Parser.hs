module Parser
  ( parseGrid,
    parseRobot,
    parseMoves,
    parseRobotAndMoves,
  )
where

import Control.Applicative (many, some)
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

parseGrid :: ByteString -> Either String Grid
parseGrid = parseOnly grid

grid :: Parser Grid
grid =
  (,)
    <$> (skipSpace *> decimal)
    <*> (skipSpace *> decimal <* skipSpace <* endOfInput)

-- n <- decimal
-- _ <- some space
-- m <- decimal
-- _ <- endOfInput
-- pure (n, m)

-- >>> parseOnly grid "2  3"
-- Right (2,3)

parseRobotAndMoves :: ByteString -> Either String (Robot, [Move])
parseRobotAndMoves = parseOnly parser
  where
    parser = (,) <$> robot <*> (some space *> moves <* endOfInput)

parseMoves :: ByteString -> Either String [Move]
parseMoves = parseOnly moves

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
  pure
    Robot
      { rPosition = (x, y),
        rOrientation = orientation'
      }
  where
    comma = skipSpace *> char ',' <* skipSpace

-- >>> parseOnly robot "(1,2,W)"
-- Left "Failed reading: Illegal orientation 'X'"

orientation :: Parser Orientation
orientation =
  anyChar >>= \case
    'N' -> pure N
    'E' -> pure E
    'S' -> pure S
    'W' -> pure W
    c -> fail $ "Illegal orientation '" ++ [c] ++ "'"
