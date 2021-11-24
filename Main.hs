module Main where

import Prelude


-- import stuff such that in GHCi we can inspect types
import System.Environment

import Elaboration
import Generator
import JSONBuilder
import Parser
import Yosys
import Nextpnr
import Steps
import Compiler

import Types


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
