{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified ParserTest (tests)
import qualified RobotV1Test (tests)
import qualified Test.Tasty as T

main :: IO ()
main = T.defaultMain tests

tests :: T.TestTree
tests =
  T.testGroup
    "Tests"
    [ RobotV1Test.tests,
      ParserTest.tests
    ]
