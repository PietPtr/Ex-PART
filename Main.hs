module Main where

import Prelude
import Compiler hiding (parse)

import System.Environment

-- Untested
main :: IO ()
main = do
    args <- getArgs
    parse args
    where
        parse [] = usage
        parse [_] = usage
        parse [flowName, dir] = make (flowMap flowName) dir
        parse _ = usage

        flowMap name = case name of
            "resource" -> resource
            "auto" -> auto
            "clean" -> clean
            "monolithic" -> monolithic
            _ -> error "Main.hs: Flow not found."

        usage =  putStrLn "Provide a flow name (clean, auto, resource, monolithic) and the name of one of the directories in examples/"