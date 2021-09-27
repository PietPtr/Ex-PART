module Yosys where

import Types
import System.Directory
import System.Process
import Data.List

import Preprocessing
import Postprocessing


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
    createProcess $ proc "yosys" ["../yosys/grouped.ys", "-v 0"] -- TODO: dit gaat echt instant stuk