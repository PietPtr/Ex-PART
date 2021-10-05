module Nextpnr where

import System.Process

import GHC.IO.Handle

import Data.List

nextpnr :: FilePath -> IO ()
nextpnr lpf = do
    processDef <- pure $ (proc "nextpnr-ecp5"
        ["--85k",
         "--json", "combined.json",
         "--lpf", lpf,
         "--textcfg", "bitstream.config",
         "--pre-place", "/usr/share/ex-part/nextpnr/constrainer.py"])
         {std_out=CreatePipe, std_err=CreatePipe}
    (_, Just outHandle, Just errHandle, processHandle) <- createProcess processDef
    stdout <- hGetContents outHandle
    stderr <- hGetContents errHandle
    _ <- waitForProcess processHandle
    writeFile ("nextpnr.log") stdout
    writeFile ("nextpnr.err") stderr
    putStr (unlines 
        $ filter (\l -> ("WARNING" `isPrefixOf` l) || ("ERROR" `isPrefixOf` l)) 
        $ lines stderr)
