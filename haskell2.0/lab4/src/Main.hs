
module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand

main :: IO ()
main = do
    putStr "miniHaskell> "
    s <- getLine
    case parseFirst replCommand s of
        Nothing -> putStr "Cannot parse">>main
        Just Quit -> return()
        Just (Load _)->putStrLn "Not implemented" >>main
        Just(Eval es) ->
            case parseFirst exprParser es of
                Nothing -> putStrLn "Error" >>main
                Just e -> putStrLn (showExp e)>> main

