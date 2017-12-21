#!/usr/bin/env stack
-- stack --resolver=lts-9.10 script
{-# OPTIONS_GHC -Wall -Werror #-}

import           Data.Char             (intToDigit)
import           Data.List             (isPrefixOf)
import           Numeric               (showHex, showIntAtBase, showOct)
import           Numeric.Natural       (Natural)
import           System.Process        (readProcess)
import           Test.Tasty            (defaultMain)
import           Test.Tasty.QuickCheck (Arbitrary, Property, arbitrary,
                                        arbitraryBoundedEnum, ioProperty,
                                        testProperty, (===))

convertProg :: FilePath
convertProg = "build/convert"

main :: IO ()
main = defaultMain $ testProperty "convert" prop_convert

data Choice = From16to10 | From8to10 | From10to2
    deriving (Bounded, Enum, Show)

instance Arbitrary Choice where
    arbitrary = arbitraryBoundedEnum

prop_convert :: Choice -> Natural -> Property
prop_convert choice n = ioProperty $ do
    got <- convert choice n
    pure $ got === prepareExpected choice n

convert :: Choice -> Natural -> IO String
convert choice n = extract <$> readProcess convertProg [] input
  where
    input = unlines [show $ choiceNum choice, prepareInput choice n, "4"]
    extract = (!! 1) . words . head . filter ("Result:" `isPrefixOf`) . lines

prepareInput :: Choice -> Natural -> String
prepareInput choice n = case choice of
    From16to10 -> showHex n ""
    From8to10  -> showOct n ""
    From10to2  -> show n

prepareExpected :: Choice -> Natural -> String
prepareExpected choice n = case choice of
    From16to10 -> show n
    From8to10  -> show n
    From10to2  -> showIntAtBase 2 intToDigit n ""

choiceNum :: Choice -> Int
choiceNum = succ . fromEnum
