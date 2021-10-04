{-# LANGUAGE BangPatterns #-}

module Yosys where

import Types
import System.Directory
import System.Process
import Data.List
import Data.Aeson
import GHC.IO.Handle

import Preprocessing
import Postprocessing


-- TODO: fix dat het kan dat on termination pas er verder gegaan wordt met de volgende actie
compileToVerilog :: Program -> IO ()
compileToVerilog (Program _ _ components) = do
    mapM_ runClash procsAndNames
    where
        cmpNames = map cmp_name components
        procsAndNames = zip cmpNames (clashProcesses cmpNames)

runClash :: (String, CreateProcess) -> IO ()
runClash (cmpName, clash) = do
    (_, Just handle, _, processHandle) <- createProcess clash
    output <- hGetContents handle
    _ <- waitForProcess processHandle
    writeFile ("builds/"++cmpName++"/clash.log") output


groupVerilogs :: Program -> IO ()
groupVerilogs (Program _ _ components) = do
    createDirectoryIfMissing True $ "builds/.grouped"
    writeFile ("builds/.grouped/build.grouped.v") ""
    mapM_ (readAndAppend) (cmpNames)
    appendFile ("builds/.grouped/build.grouped.v") (dummyTop components)
    where
        cmpNames = map cmp_name components

-- synthesizeTop :: IO ()
synthesizeTop = do
    createProcess $ proc "yosys" ["-q", "-d", "../yosys/grouped.ys"]

customConnect program system = 
    encodeFile ("interconnect.json") (makeTopModule program system)

combineJSONs = do
    createProcess $ proc "python3" ["../yosys/merge_json.py"] -- TODO: dit is ook kut op twee levels