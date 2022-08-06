{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString as B (getLine)
import Parser (parseGrid, parseRobotAndMoves)
import Robot
  ( Grid,
    RobotState (..),
    move,
    prettyState,
  )

main :: IO ()
main = go
  where
    go =
      parseGridLine >>= \case
        Left err -> print err >> go
        Right grid -> do
          -- This is static at the moment but we could decide to allow more than
          -- two robots to be entered. It'd be easy to change this by simply
          -- reading the number of robots from the input.
          let robotCount = 2 :: Int
          -- Read all the robots and moves
          robots <- forM [0 .. robotCount - 1] (const $ fmap parseRobotAndMoves B.getLine)
          -- Apply moves to the robots or print an error if input is invalid
          forM_ robots \case
            Left err -> print err
            Right (robot, movements) -> do
              let outcome = move movements grid (Located robot)
              print $ prettyState outcome
          go

    parseGridLine :: IO (Either String Grid)
    parseGridLine = fmap parseGrid B.getLine
