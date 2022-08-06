{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module RobotTest (tests) where

import GenericRobot
  ( Orientation (..),
    RobotState (..),
    SimpleMovement (..),
    SimpleRobot (..),
    executeMission,
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
        let grid = (4, 8)
            initRobot = SimpleRobot (2, 3) E
            actual = executeMission [L, F, R, F, F] (grid, Located initRobot)
            expected = Located (SimpleRobot (4, 4) E)
        actual H.@?= expected,
      H.testCase "Mars rover found (2)" do
        let grid = (4, 8)
            initRobot = SimpleRobot (2, 3) N
            actual = executeMission [F, L, L, F, R] (grid, Located initRobot)
            expected = Located (SimpleRobot (2, 3) W)
        actual H.@?= expected,
      H.testCase "Mars rover lost (1)" do
        let grid = (4, 8)
            initRobot = SimpleRobot (0, 2) N
            actual = executeMission [F, F, L, F, R, F, F] (grid, Located initRobot)
            expected = Lost (SimpleRobot (0, 4) W)
        actual H.@?= expected,
      H.testCase "Mars rover lost (2)" do
        let grid = (4, 8)
            initRobot = SimpleRobot (1, 0) S
            actual = executeMission [F, F, R, L, F] (grid, Located initRobot)
            expected = Lost (SimpleRobot (1, 0) S)
        actual H.@?= expected,
      ignoreTest $ H.testCase "[TODO] Mars rover movements" do
        H.assertBool "" False
    ]
