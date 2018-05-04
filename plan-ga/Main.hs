{-# OPTIONS -Wno-partial-type-signatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Debug.Trace

import           AI.GeneticAlgorithm.Simple
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable
import           GHC.Generics (Generic)
import           Graphics.Gloss
import           System.Random (Random, RandomGen, StdGen, newStdGen, random,
                                randomR)

type Time = Float

type WorkId = String

type Resource = Float

data Work = Work
    { workId       :: WorkId
    , duration     :: Time
    , resourceCost :: Resource
    , chainId      :: ChainId
    }
    deriving (Eq, Generic, NFData, Ord, Show)

-- | List of chains
type Chain = [Work]
type Chains = [Chain]

type ChainId = Int
data Step = Wait | Run ChainId
    deriving (Generic, NFData, Show)
type Plan = [Step]

data Env = Env
    { envChains        :: Chains
    , envResourceLimit :: Resource
    }
    deriving (Generic, NFData)

type Schedule = Map Time (Set Work)

randomWork :: WorkId -> ChainId -> State StdGen Work
randomWork workId chainId = do
    duration <- randomRS (1, 10)
    resourceCost <- randomRS (2, 10)
    pure Work{..}

scheduleEndTime :: Schedule -> Time
scheduleEndTime schedule
    | null schedule = 0
    | otherwise     = maximum
        [ start + duration
        | (start, works) <- Map.assocs schedule, Work{duration} <- toList works
        ]

randomPlan :: Chains -> State StdGen Plan
randomPlan chains = case chains of
    [] -> pure []
    _  -> do
        b <- randomRS (False, True)
        if b then do
            i <- randomRS (0, chainCount - 1)
            let (_, chains') = popWork i chains
            (Run i :) <$> randomPlan chains'
        else
            (Wait :) <$> randomPlan chains
  where
    chainCount = length chains

popWork :: ChainId -> Chains -> (Work, Chains)
popWork i chains =
    case chains !! i of
        []         -> error "empty chain"
        [work]     -> (work, take i chains ++         drop (i + 1) chains)
        work:chain -> (work, take i chains ++ chain : drop (i + 1) chains)

twist :: [a] -> [a] -> [a]
twist [] ys     = ys
twist (x:xs) ys = x : twist ys xs

instance Chromosome (Env, Plan) where
    crossover g (env, plan1) (_, plan2) =
        (map (env,) [plan1, plan2, twist plan1 plan2, twist plan2 plan1], g)

    mutation g (env, plan) = (runState ?? g) . fmap (env,) $ do
        i <- randomRS (0, length plan - 1)
        d <- randomRS (False, True)
        if d then do
            -- insert step
            b <- randomRS (False, True)
            step <-
                if b then
                    Run <$> state random
                else
                    pure Wait
            pure $ take i plan ++ step : drop i plan
        else
            -- remove step
            pure $ take i plan ++ drop (i + 1) plan

    fitness (env, plan) = case runPlan env plan of
        Just schedule ->
            1
            / realToFrac (scheduleEndTime schedule)
            / fromIntegral (length plan + 1)
        Nothing -> 0

runPlan :: Env -> Plan -> Maybe Schedule
runPlan Env{envChains, envResourceLimit} plan =
    evalState ?? (envChains, Map.empty, startTime, Map.empty) $ runMaybeT $ do
        for_ plan go
        finalize
        use schedule

  where
    chains   :: Lens' _ Chains
    chains   = _1
    jobs     :: Lens' _ (Map Time (Set Work))
    jobs     = _2
    time     :: Lens' _ Time
    time     = _3
    schedule :: Lens' _ Schedule
    schedule = _4

    startTime = 0 :: Time

    go Wait = do
        endingJobs <- uses jobs Map.minViewWithKey
        case endingJobs of
            Nothing                -> pure ()
            Just ((end, _), continuingJobs) -> do
                time .= end
                jobs .= continuingJobs

    go (Run ch) = do
        continuingChains <- use chains
        case continuingChains of
            [] -> pure ()
            _  -> do
                curTime <- use time
                let endTime = curTime + duration work
                chains .= continuingChains'
                schedule .@ curTime <>= Set.singleton work
                jobs     .@ endTime <>= Set.singleton work
                checkResources
              where
                (work, continuingChains') = popWork i continuingChains
                i = ch `mod` chainCount
                chainCount = length continuingChains

    finalize = do
        chainsLeft <- use chains
        lastTime   <- use time
        schedule .@ lastTime <>= Set.fromList (concat chainsLeft)
        jobs     .@ lastTime <>= Set.fromList (concat chainsLeft)
        checkResources

    checkResources = do
        activeJobs <- use jobs
        let resources = sum
                [ resourceCost
                | works <- toList activeJobs
                , Work{resourceCost} <- toList works
                ]
        traceShowM (activeJobs, resources)
        guard (resources <= envResourceLimit)

-- | 'at' with 'mempty' as default.
(.@) :: (Ord k, Eq m, Monoid m) => Lens' a (Map k m) -> k -> Lens' a m
focusMap .@ key = focusMap . at key . non mempty

display' :: Schedule -> IO ()
display' schedule = display window white $ translate dx dy pic
  where
    window = InWindow title size (0, 0)
    (pic, size@(width, _)) = drawSchedule schedule
    title = "Planning with genetic algorithm"
    dx = - fromIntegral width / 2
    dy = 0

drawSchedule :: Schedule -> (Picture, (Int, Int))
drawSchedule schedule =
    (pic, (round width, round $ workBlockHeight * fromIntegral chainCount))
  where
    xscale = 50
    pic = (`foldMap` Map.assocs schedule) $ \(start, works) -> mconcat
        [ translate
            (xscale * (start + duration / 2))
            (negate $ (fromIntegral chainId - 0.5) * workBlockHeight)
            (   scale xscale 1
                    (   color semitransparentBlue (rectangleSolid w h)
                    <>  rectangleWire duration workBlockHeight
                    )
            <>  scale fontSize fontSize (text workId)
            )
        | Work{duration, workId, chainId, resourceCost} <- toList works
        , let
            w = duration
            h = workBlockHeight * resourceCost / globalResourceLimit
        ]
    width = xscale * scheduleEndTime schedule
    semitransparentBlue = makeColor 0 0 1 0.33
    fontSize = 0.1
    chainCount =
        1 +
        maximum
            [chainId | works <- toList schedule, Work{chainId} <- toList works]

workBlockHeight :: Float
workBlockHeight = 200

globalResourceLimit :: Resource
globalResourceLimit = 10

randomRS :: (Random a, RandomGen g) => (a, a) -> State g a
randomRS = state . randomR

main :: IO ()
main = do
    gen <- newStdGen

    let chains =
            evalState ?? (0 :: Int, gen) $
            for [0..1] $ \chainId ->
            replicateM 3 $ do
                n <- _1 <<+= 1
                zoom _2 $ randomWork (show n) chainId
    let env = Env{envChains = chains, envResourceLimit = globalResourceLimit}
    putStrLn "chains ="
    for_ chains $ \chain -> do
        putStr "\t"
        print chain
    let newChromosome g = ((env, plan), g') where
            (plan, g') = runState (randomPlan chains) g

    -- solution
    let (_, plan) =
            runGA gen populationSize mutationChance newChromosome stop
    let schedule = runPlan env plan
    putStrLn $ "*** plan = " ++ show plan
    putStrLn $ "*** schedule = " ++ show schedule
    display' $ fromMaybe (error "invalid plan") schedule

  where
    mutationChance = 0.5
    populationSize = 10
    stop _ count = count > 10
