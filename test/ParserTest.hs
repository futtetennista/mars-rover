module ParserTest (tests) where

import Data.ByteString (ByteString, append)
import Data.ByteString.Char8 (pack)
import Data.Either (isLeft)
import Data.List (intercalate)
import Parser (parseGrid, parseRobot, parseRobotAndMoves)
import Robot (Grid, Movement (..), Orientation (..), Robot (..))
import Test.QuickCheck (Arbitrary)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as H
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.ExpectedFailure (ignoreTest)

newtype SomeGrid = SomeGrid (ByteString, Grid)
  deriving (Show)

instance Arbitrary SomeGrid where
  arbitrary = do
    n <- QC.chooseInt (1, maxBound)
    m <- QC.chooseInt (1, maxBound)
    k <- QC.chooseInt (1, 5)
    let spaces = replicate k ' '
        gridString = show n ++ spaces ++ show m
    pure $ SomeGrid (pack gridString, (n, m))

newtype ValidRobot = ValidRobot (ByteString, Robot)
  deriving (Show)

instance Arbitrary ValidRobot where
  arbitrary = do
    x <- QC.chooseInt (1, maxBound)
    y <- QC.chooseInt (1, maxBound)
    k <- QC.chooseInt (1, 5)
    o <- QC.elements [minBound ..]
    let comma = replicate k ' ' ++ ","
        robotString = "(" ++ intercalate comma [show x, show y, show o] ++ ")"
    pure $ ValidRobot (pack robotString, Robot (x, y) o)

newtype ValidMovements = ValidMovements (ByteString, [Movement])
  deriving (Show)

instance Arbitrary ValidMovements where
  arbitrary = do
    n <- QC.chooseInt (1, 100)
    moves <- QC.vectorOf n $ QC.chooseEnum @Movement (minBound, maxBound)
    let movesString = foldMap show moves
    pure $ ValidMovements (pack movesString, moves)

tests :: T.TestTree
tests =
  T.testGroup
    "Parser Tests"
    [ QC.testProperty "Parse valid grid" \(SomeGrid (input, expectedGrid)) ->
        parseGrid input == Right expectedGrid,

      H.testCase "Parse invalid grid fails" do
        H.assertBool
          "Expected parsing to fail"
          (isLeft $ parseGrid "4  -8"),

      H.testCase "Parse grid with 0 bounds fails" do
        H.assertBool "Expected parsing to fail" $ isLeft $ parseGrid "1 0"
        H.assertBool "Expected parsing to fail" $ isLeft $ parseGrid "0 1"
        H.assertBool "Expected parsing to fail" $ isLeft $ parseGrid "0 0",

      ignoreTest $ H.testCase "Parse grid with bounds too big 0 fails" do
        H.assertBool "Expected parsing to fail" False,

      H.testProperty "Parse valid robot" \(ValidRobot (input, expectedRobot)) ->
        parseRobot input == Right expectedRobot,

      H.testCase "Parse robot with 0 position" do
        H.assertBool "Expected parsing to succeed" $ parseRobot "(0, 0, W)" == Right (Robot (0, 0) W)
        H.assertBool "Expected parsing to succeed" $ parseRobot "(0, 1, W)" == Right (Robot (0, 1) W)
        H.assertBool "Expected parsing to succeed" $ parseRobot "(1, 0, W)" == Right (Robot (1, 0) W),

      H.testCase "Parse invalid robot" do
        H.assertBool
          "Expected parsing to fail"
          (isLeft $ parseRobot "(2, 3, X)"),

      H.testProperty "Parse robot and moves" $
        QC.forAll robotAndMoves \(input, expectedRobot, expectedMoves) ->
            parseRobotAndMoves input == Right (expectedRobot, expectedMoves)
    ]
  where
    robotAndMoves = do
      ValidRobot (robotString, expectedRobot) <- QC.arbitrary
      ValidMovements (movesString, expectedMoves) <- QC.arbitrary
      let input = robotString `append` " " `append` movesString
      pure (input, expectedRobot, expectedMoves)
