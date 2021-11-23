module Flows where

import Types
import Steps

import Data.Either
import Data.Aeson
import Data.Text (pack, unpack)
import Data.List
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
import System.Directory
import Data.List.Split

-- These compile flows change directory very often. When one crashes the program may not
-- be in the directory you expect. Run :r in ghci to return to the original one.


clean :: Program -> System -> FilePath -> FilePath -> FilePath -> IO ()
clean expc_rolled expi_rolled expcPath expiPath outDir = do
    createDirAndEnter outDir

    (expc, expi) <- unrollRepetitions expc_rolled expi_rolled

    writeLocations expi
    generateClash expc
    flattenForSim expc expi
    compileToVerilog expc
    groupVerilogFiles expc
    synthesizeComponents
    connectComponents expc expi


expcChanged :: Program -> System -> [Component] -> [Component] -> [Component] -> IO ()
expcChanged expc_rolled expi_rolled changed deleted newcmps = do
    expc_current <- pure $ expc_rolled {prg_cmps=(changed ++ newcmps)}
    (expc, expi) <- unrollRepetitions expc_rolled expi_rolled

    writeLocations expi
    generateClash expc
    flattenForSim expc expi
    compileToVerilog expc
    removeDeleted deleted
    groupVerilogFiles expc
    synthesizeComponents
    connectComponents expc expi

expiChanged :: Program -> System -> IO ()
expiChanged expc_rolled expi_rolled = do
    (expc, expi) <- unrollRepetitions expc_rolled expi_rolled

    writeLocations expi
    flattenForSim expc expi
    connectComponents expc expi


monolithic :: Program -> System -> FilePath -> FilePath -> IO ()
monolithic expc_rolled expi_rolled lpfPath outDir = do
    let outDir' = (slashscrape outDir) ++ "_monolithic"

    createDirAndEnter outDir'
    (expc, expi) <- unrollRepetitions expc_rolled expi_rolled

    generateClash expc
    flattenForSim expc expi
    monolithicToVerilog
    synthesizeMonolithic
    noConstraintPnR lpfPath

    where
        slashscrape "" = ""
        slashscrape p = if last p == '/' then init (slashscrape p) else p

-- Compile flow that synthesizes/place & routes everything of all components separately s.t.
-- resource usage of individual components can be analyzed
-- Can be used as clean build, and after folder has been made.
resource :: Program -> FilePath -> FilePath -> IO ()
resource expc lpfPath outDir = do
    lpfLoc <- makeAbsolute lpfPath
    
    createDirAndEnter outDir
    generateClash expc
    compileToVerilog expc
    synthesizeAndPnRIndividualComponents lpfLoc


    