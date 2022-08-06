{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module RobotTest (tests) where

import Robot
  ( Movement (..),
    Orientation (..),
    Robot (..),
    RobotState (..),
    move,
  )
import qualified Test.Tasty as T
import Test.Tasty.ExpectedFailure (ignoreTest)
import qualified Test.Tasty.HUnit as H

-- import qualified Test.Tasty.QuickCheck as QC

tests :: T.TestTree
tests =
  T.testGroup
    "Tests"
    [ H.testCase "Mars rover found (1)" do
        let movements = [L, F, R, F, F]
            grid = (4, 8)
            initRobot = Robot (2, 3) E
            actual = move movements grid (Located initRobot)
            expected = Located (Robot (4, 4) E)
        actual H.@?= expected,
      H.testCase "Mars rover found (2)" do
        let movements = [F, L, L, F, R]
            grid = (4, 8)
            initRobot = Robot (2, 3) N
            actual = move movements grid (Located initRobot)
            expected = Located (Robot (2, 3) W)
        actual H.@?= expected,
      H.testCase "Mars rover lost (1)" do
        let movements = [F, F, L, F, R, F, F]
            grid = (4, 8)
            initRobot = Robot (0, 2) N
            actual = move movements grid (Located initRobot)
            expected = Lost (Robot (0, 4) W)
        actual H.@?= expected,
      H.testCase "Mars rover lost (2)" do
        let movements = [F, F, R, L, F]
            grid = (4, 8)
            initRobot = Robot (1, 0) S
            actual = move movements grid (Located initRobot)
            expected = Lost (Robot (1, 0) S)
        actual H.@?= expected,
      ignoreTest $ H.testCase "[TODO] Mars rover movements" do
        H.assertBool "" False
    ]
