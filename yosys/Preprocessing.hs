module Preprocessing where

import System.Directory
import System.Process

import Types

-- assumes that every folder in builds except ".grouped" needs to be compiled
-- compileToVerilog :: FilePath -> IO ()
compileToVerilog basedir (Program _ _ components) = mapM_ createProcess $ clashProcesses basedir cmpNames
    where
        cmpNames = map cmp_name components
   

-- TODO: fragiel.
clashProcesses :: FilePath -> [String] -> [CreateProcess]
clashProcesses basedir cmpNames = map 
    (\name -> proc "clash" [basedir ++ "/builds/"++name++"/Synth_" ++ name ++ ".hs", "--verilog", "-outputdir "++basedir++"/buildfiles", "-fclash-hdldir hdl", "-i" ++ basedir]) 
    cmpNames


