module Main where

import Prelude hiding (repeat)


import Compiler

import Types
import Parser
import Unroll
import Generator
import JSONBuilder
import Yosys
import Nextpnr
import Compiler

import System.Directory
import System.Environment

-- Untested
main = do
    args <- getArgs
    parse args
    where
        parse [] = usage
        parse [x] = usage
        parse [flowName, dir] = make (flowMap flowName) dir
        parse _ = usage

        flowMap name = case name of
            "resource" -> resource
            "auto" -> auto
            "clean" -> clean
            "monolithic" -> monolithic
            _ -> error "Main.hs: Flow not found."

        usage =  putStrLn "Provide a flow name (clean, auto, resource, monolithic) and the name of one of the directories in examples/"