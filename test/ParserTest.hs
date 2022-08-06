module ParserTest (tests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Either (isLeft)
import Data.List (intercalate)
import Parser (parseGrid, parseRobot)
import RobotV1 (Grid, Robot (..))
import Test.QuickCheck (Arbitrary)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as H
import qualified Test.Tasty.QuickCheck as QC

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

tests :: T.TestTree
tests =
  T.testGroup
    "Parser Tests"
    [ QC.testProperty "Test grid parsing" \(SomeGrid (gridString, expectedGrid)) ->
        parseGrid gridString == Right expectedGrid,
      H.testCase "Test grid parsing rejects negative numbers" do
        H.assertBool
          "Expected parsing to fail"
          (isLeft $ parseGrid "4  -8"),
      H.testProperty "Test robot parsing" \(ValidRobot (robotString, expectedRobot)) ->
        parseRobot robotString == Right expectedRobot,
      H.testCase "Test robot parsing fails" do
        H.assertBool
          "Expected parsing to fail"
          (isLeft $ parseRobot "(2, 3, X)")
    ]
