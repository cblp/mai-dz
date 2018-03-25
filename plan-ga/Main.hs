{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.QuickCheck

data Work = Work
    { timeCost :: Double -- time cost, in time units
    , resourceCost :: Double -- resource cost, in resource units
    }
    deriving Show

instance Arbitrary Work where
    arbitrary = do
        Positive timeCost <- arbitrary
        Positive resourceCost <- arbitrary
        pure Work{..}

main :: IO ()
main = do
    work :: Work <- generate arbitrary
    print work
