{-# LANGUAGE TypeApplications #-}

import           Data.Maybe (mapMaybe)
import           System.Environment (getArgs, getProgName)
import           Text.Read (readMaybe)

main :: IO ()
main = do
    prog   <- getProgName
    args   <- getArgs
    (a, b) <- case mapMaybe (readMaybe @Double) args of
        sides@[a, b] | length args == 2, all (> 0) sides ->
            pure (a, b)
        _ -> fail ("Usage: " ++ prog ++ " NUMBER NUMBER")
    putStrLn ("perimeter: " ++ show (2 * (a + b)))
    putStrLn ("diagonal: " ++ show (sqrt (a * a + b * b)))
