module Flows where

import Types
import Steps

import System.Directory

-- These compile flows change directory very often. When one crashes the program may not
-- be in the directory you expect. Run :r in ghci to return to the original one.


clean :: System -> FilePath -> IO ()
clean system outDir = do
    createDirAndEnter outDir

    writeLocations system
    generateClash system
    flattenForSim system
    compileToVerilog system
    groupVerilogFiles system
    synthesizeComponents
    connectComponents system


expcChanged :: System -> [Component] -> [Component] -> [Component] -> IO ()
expcChanged top changed deleted newcmps = do
    -- TODO: !! this is not automagically fixed
    -- expc_current <- pure $ expc_rolled {prg_cmps=(changed ++ newcmps)}
    top' <- pure $ top {sys_topdata=(sys_topdata top) {top_cmps=(changed ++ newcmps)}}

    writeLocations top'
    generateClash top'
    flattenForSim top'
    compileToVerilog top'
    removeDeleted deleted
    groupVerilogFiles top'
    synthesizeComponents
    connectComponents top'

expiChanged :: System -> IO ()
expiChanged top = do
    writeLocations top
    flattenForSim top
    connectComponents top


monolithic :: System -> FilePath -> FilePath -> IO ()
monolithic system lpfPath outDir = do
    let outDir' = (slashscrape outDir) ++ "_monolithic"

    createDirAndEnter outDir'

    generateClash system
    flattenForSim system
    monolithicToVerilog
    synthesizeMonolithic
    noConstraintPnR lpfPath

    where
        slashscrape "" = ""
        slashscrape p = if last p == '/' then init (slashscrape p) else p

-- Compile flow that synthesizes/place & routes everything of all components separately s.t.
-- resource usage of individual components can be analyzed
-- Can be used as clean build, and after folder has been made.
resource :: System -> FilePath -> FilePath -> IO ()
resource system lpfPath outDir = do
    lpfLoc <- makeAbsolute lpfPath
    
    createDirAndEnter outDir
    generateClash system
    compileToVerilog system
    synthesizeAndPnRIndividualComponents lpfLoc


    