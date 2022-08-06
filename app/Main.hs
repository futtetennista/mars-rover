{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString as B (getLine)
import GenericRobot
  ( RobotParser (parseGrid, parseRobotAndMovements),
    RobotState (Located),
    SimpleGrid,
    SimpleRobot,
    executeMission,
    prettyState,
  )
import Text.Read (readMaybe)

main :: IO ()
main = go
  where
    go =
      parseGridLine >>= \case
        Left err -> print err >> go
        Right grid ->
          readMaybe @Word <$> getLine >>= \case
            Nothing -> print "Please provide a positive number of robots" >> go
            Just robotCount -> do
              -- Read all the robots and moves
              robots <- forM [0 .. robotCount - 1] parseRobotAndMovementsLine
              -- Apply moves to the robots or print an error if input is invalid
              forM_ robots \case
                Left err -> print err
                Right (robot, inputMovements) -> do
                  let outcome = executeMission inputMovements (grid, Located robot)
                  print $ prettyState outcome
              go

    parseGridLine :: IO (Either String SimpleGrid)
    parseGridLine = parseGrid @SimpleRobot <$> B.getLine

    parseRobotAndMovementsLine _ =
      parseRobotAndMovements @SimpleRobot <$> B.getLine
