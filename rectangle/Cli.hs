import           Data.Maybe (mapMaybe)
import           System.Environment (getArgs, getProgName)
import           Text.Read (readMaybe)

main :: IO ()
main = do
    prog   <- getProgName
    args   <- getArgs
    (a, b) <- case mapMaybe readMaybe args of
        [a, b] -> pure (a, b :: Double)
        _      -> fail ("Usage: " ++ prog ++ " NUMBER NUMBER")
    putStrLn ("perimeter: " ++ show (2 * (a + b)))
    putStrLn ("diagonal: " ++ show (sqrt (a * a + b * b)))
