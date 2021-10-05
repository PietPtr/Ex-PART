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
    (_, Just outHandle, Just errHandle, processHandle) <- createProcess clash
    stdout <- hGetContents outHandle
    stderr <- hGetContents errHandle
    _ <- waitForProcess processHandle
    writeFile ("builds/"++cmpName++"/clash.log") stdout
    writeFile ("builds/"++cmpName++"/clash.err") stderr


groupVerilogs :: Program -> IO ()
groupVerilogs (Program _ _ components) = do
    createDirectoryIfMissing True $ "builds/.grouped"
    writeFile ("builds/.grouped/build.grouped.v") ""
    mapM_ (readAndAppend) (cmpNames)
    appendFile ("builds/.grouped/build.grouped.v") (dummyTop components)
    where
        cmpNames = map cmp_name components

synthesizeTop :: IO ()
synthesizeTop = do
    (_, Just outHandle, _, processHandle) <- createProcess $ 
        (proc "yosys" ["/usr/share/ex-part/yosys/grouped.ys"])
        {std_err=CreatePipe, std_out=CreatePipe}
    stdout <- hGetContents outHandle
    writeFile "builds/.grouped/yosys.log" stdout

customConnect program system = 
    encodeFile "interconnect.json" (makeTopModule program system)

combineJSONs basedir = do
    (_, _, _, h) <- createProcess $ 
        proc "python3" ["/usr/share/ex-part/yosys/merge_json.py", basedir]
    waitForProcess h
