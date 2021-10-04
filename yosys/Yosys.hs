{-# LANGUAGE BangPatterns #-}

module Yosys where

import Types
import System.Directory
import System.Process
import Data.List
import Data.Aeson

import Preprocessing
import Postprocessing


-- TODO: fix dat het kan dat on termination pas er verder gegaan wordt met de volgende actie
compileToVerilog basedir (Program _ _ components) = 
    mapM_ createProcess $ clashProcesses basedir cmpNames
    where
        cmpNames = map cmp_name components

groupVerilogs basedir (Program _ _ components) = do
    createDirectoryIfMissing True $ basedir ++ "/builds/.grouped"
    writeFile (basedir ++ "/builds/.grouped/build.grouped.v") ""
    mapM_ (readAndAppend basedir) (cmpNames)
    appendFile (basedir ++ "/builds/.grouped/build.grouped.v") (dummyTop components)
    where
        cmpNames = map cmp_name components

synthesizeTop basedir = do
    setCurrentDirectory basedir
    createProcess $ proc "yosys" ["-q", "-d", "../yosys/grouped.ys"] -- TODO: dit gaat echt instant stuk
    setCurrentDirectory ".."

customConnect basedir program system = 
    encodeFile (basedir ++ "/interconnect.json") (makeTopModule program system)

combineJSONs basedir = do
    createProcess $ proc "python3" ["yosys/merge_json.py", basedir] -- TODO: dit is ook kut op twee levels