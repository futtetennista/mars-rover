{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module RobotV1Test (tests) where

import RobotV1
  ( Move (..),
    Orientation (..),
    Robot (..),
    RobotState (..),
    moveRobot,
  )
import qualified Test.Tasty as T
import Test.Tasty.ExpectedFailure (ignoreTest)
import qualified Test.Tasty.HUnit as H

-- import qualified Test.Tasty.QuickCheck as QC

tests :: T.TestTree
tests =
  T.testGroup
    "Tests"
    [ H.testCase "Mars rover found" do
        let moves = [L, F, R, F, F]
            grid = (4, 8)
            initRobot = Robot (2, 3) E
            actual = moveRobot moves grid (Found initRobot)
            expected = Found (Robot (4, 4) E)
        actual H.@?= expected,
      H.testCase "Mars rover lost" do
        let moves = [F, F, L, F, R, F, F]
            grid = (4, 8)
            initRobot = Robot (0, 2) N
            actual = moveRobot moves grid (Found initRobot)
            expected = Lost (Robot (0, 4) W)
        actual H.@?= expected,
      ignoreTest $ H.testCase "Mars rover movements" do
        H.assertBool "" False
    ]
