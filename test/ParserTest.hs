module ParserTest (tests) where

import Data.ByteString (ByteString, append)
import Data.ByteString.Char8 (pack)
import Data.Either (isLeft)
import Data.List (intercalate)
import GenericRobot (SimpleRobot (..), SimpleMovement (..), SimpleGrid, parseGrid, parseRobotAndMovements)
-- import Robot (Grid, Movement (..), Robot (..))
import Test.QuickCheck (Arbitrary)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as H
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.ExpectedFailure (ignoreTest)

newtype SomeGrid = SomeGrid (ByteString, SimpleGrid)
  deriving (Show)

instance Arbitrary SomeGrid where
  arbitrary = do
    n <- QC.chooseInt (1, maxBound)
    m <- QC.chooseInt (1, maxBound)
    k <- QC.chooseInt (1, 5)
    let spaces = replicate k ' '
        gridString = show n ++ spaces ++ show m
    pure $ SomeGrid (pack gridString, (n, m))

newtype ValidRobot = ValidRobot (ByteString, SimpleRobot)
  deriving (Show)

instance Arbitrary ValidRobot where
  arbitrary = do
    x <- QC.chooseInt (1, maxBound)
    y <- QC.chooseInt (1, maxBound)
    k <- QC.chooseInt (1, 5)
    o <- QC.elements [minBound ..]
    let comma = replicate k ' ' ++ ","
        robotString =
          "(" ++ intercalate comma [show x, show y, show o] ++ ")"
    pure $ ValidRobot (pack robotString, SimpleRobot (x, y) o)

newtype ValidMovements = ValidMovements (ByteString, [SimpleMovement])
  deriving (Show)

instance Arbitrary ValidMovements where
  arbitrary = do
    n <- QC.chooseInt (1, 100)
    moves <- QC.vectorOf n $ QC.chooseEnum @SimpleMovement (minBound, maxBound)
    let movesString = foldMap show moves
    pure $ ValidMovements (pack movesString, moves)

tests :: T.TestTree
tests =
  T.testGroup
    "Parser Tests"
    [ QC.testProperty "Parse valid grid" \(SomeGrid (input, expectedGrid)) ->
        parseGrid @SimpleRobot input == Right expectedGrid,

      H.testCase "Parse invalid grid fails" do
        H.assertBool
          "Expected parsing to fail"
          (isLeft $ parseGrid @SimpleRobot "4  -8"),

      ignoreTest $ H.testCase "Parse grid with 0 bounds fails" do
        H.assertBool "Expected parsing to fail" False,

      ignoreTest $ H.testCase "Parse grid with bounds too big 0 fails" do
        H.assertBool "Expected parsing to fail" False,

      H.testProperty "Parse valid robot" do
        QC.forAll QC.arbitrary \(ValidRobot (input, expectedRobot)) ->
          QC.ioProperty do
            let input' = input `append` " FLR"
            pure $ (fst <$> parseRobotAndMovements input') == Right expectedRobot,

      H.testCase "Parse invalid robot" do
        H.assertBool
          "Expected parsing to fail"
          (isLeft $ parseRobotAndMovements @SimpleRobot "(2, 3, X) FLR"),

      H.testProperty "Parse robot and moves" $
        QC.forAll robotAndMoves \(input, expectedRobot, expectedMoves) ->
          parseRobotAndMovements input == Right (expectedRobot, expectedMoves)
    ]
  where
    robotAndMoves = do
      ValidRobot (robotString, expectedRobot) <- QC.arbitrary
      ValidMovements (movesString, expectedMoves) <- QC.arbitrary
      let input = robotString `append` " " `append` movesString
      pure (input, expectedRobot, expectedMoves)
