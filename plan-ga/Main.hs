{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import           AI.GeneticAlgorithm.Simple
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           GHC.Generics
import           Graphics.Gloss
import           System.Random

type Time = Int

data Task = Task
    { duration     :: Time  -- in time units
    , resourceCost :: Int   -- in resource units
    }
    deriving (Eq, Generic, NFData, Ord, Show)

data Work = Work
    { task      :: Task
    , startTime :: Time
    }
    deriving (Eq, Generic, NFData, Ord, Show)

endTime :: Work -> Time
endTime Work{startTime, task = Task{duration}} = startTime + duration

type Schedule = [Work]

randomTask :: State StdGen Task
randomTask = do
    duration <- randomRS (1, 100)
    resourceCost <- randomRS (1, 100)
    pure Task{..}

randomSchedule :: [Task] -> State StdGen Schedule
randomSchedule = traverse $ \task -> do
    startTime <- randomRS (1, 1000)
    pure Work{task, startTime}

instance Chromosome Schedule where
    crossover g schedule1 schedule2 = ([schedule], g)
      where
        schedule =
            [ Work{startTime = (st1 + st2) `div` 2, task}
            | Work{startTime = st1, task} <- schedule1
            | Work{startTime = st2} <- schedule2
            ]

    mutation g schedule = (`runState` g) $ do
        i <- randomRS (0, length schedule - 1)
        d <- randomRS (0, 5 :: Int) <&> \case
            0 -> -100
            1 -> -10
            2 -> -1
            3 -> 1
            4 -> 10
            _ -> 100
        pure $ case splitAt i schedule of
            (before, work : after) ->
                before ++ work{startTime = max 0 $ startTime work + d} : after
            _ -> error "empty schedule"

    fitness schedule =
        1
        / (1 + fromIntegral timeToStart)
        / (1 + fromIntegral totalTime)
        / (1 + fromIntegral intersectionMeasure) ^ (2 :: Int)
      where
        timeToStart = minimum (map startTime schedule)
        totalTime = maximum (map endTime schedule)
        intersectionMeasure = sum
            [ max 0 (min end1 end2 - max start1 start2)
            | workTails <- tails schedule
            , work1 : workTail <- pure workTails
            , work2 <- workTail
            , let start1 = startTime work1
                  end1   = endTime   work1
                  start2 = startTime work2
                  end2   = endTime   work2
            ]

draw :: Schedule -> Picture
draw = foldMap drawWork
  where
    drawWork Work{task = Task{duration}, startTime} =
        translate (fromIntegral $ startTime + duration `div` 2) 0
        . color (makeColor 0 0 1 0.33)
        $ rectangleSolid (fromIntegral duration) workBlockHeight

main :: IO ()
main = do
    g <- newStdGen

    let tasks = evalState (replicateM 10 randomTask) g
    putStrLn $ "tasks = " ++ show tasks
    -- let solution0 = [Work{task, startTime = 0} | task <- tasks]
    -- display' solution0

    let solution =
            runGA g populationSize 0.5 (runState $ randomSchedule tasks) stop
    putStrLn $ "solution = " ++ show solution
    putStrLn $ "min startTime = " ++ show (minimum $ map startTime solution)
    putStrLn $ "max endTime = " ++ show (maximum $ map endTime solution)
    display' solution

  where
    populationSize = 30
    stop _ count = count > 200

display' :: Schedule -> IO ()
display' schedule = display window white $ translate dx dy pic
  where
    window = InWindow title (width, round height) (0, 0)
    pic = draw schedule
    title = "Planning with genetic algorithm"
    height = workBlockHeight
    width = maximum $ map endTime schedule
    dx = - fromIntegral width / 2
    dy = 0

workBlockHeight :: Float
workBlockHeight = 100

randomRS :: (Random a, RandomGen g) => (a, a) -> State g a
randomRS = state . randomR
