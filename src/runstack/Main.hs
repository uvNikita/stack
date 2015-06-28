module Main where

import System.Environment
import System.Exit
import System.Process

main :: IO ()
main = do
    args <- getArgs
    fp <-
        case args of
            [fp] -> return fp
            [] -> error "Must provide a filename"
            _ -> error "Must provide exactly one filename"
    ls <- fmap lines $ readFile fp
    let stackArgs = takeArgs $ dropShebang ls
    let cp = proc "stack" $ stackArgs ++ [fp]
    (Nothing, Nothing, Nothing, ph) <- createProcess cp
    waitForProcess ph >>= exitWith

dropShebang :: [String] -> [String]
dropShebang (('#':'!':_):rest) = rest
dropShebang x = x

takeArgs :: [String] -> [String]
takeArgs [] = []
takeArgs (x:_) =
    case words x of
        "--":"stack":rest -> rest
        _ -> []
