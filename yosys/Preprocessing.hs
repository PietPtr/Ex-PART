module Preprocessing where

import System.Directory
import System.Process
import Data.List

import Types

-- assumes that every folder in builds except ".grouped" needs to be compiled
-- compileToVerilog :: FilePath -> IO ()
compileToVerilog basedir (Program _ _ components) = 
    mapM_ createProcess $ clashProcesses basedir cmpNames
    where
        cmpNames = map cmp_name components

-- TODO: fragiel.
clashProcesses :: FilePath -> [String] -> [CreateProcess]
clashProcesses basedir cmpNames = map 
    (\name -> proc "clash" [
        basedir ++ "/builds/"++name++"/Synth_" ++ name ++ ".hs", 
        "--verilog", 
        "-outputdir "++basedir++"/.hs", 
        "-fclash-hdldir "++basedir ++ "/builds/"++name++"/hdl", 
        "-i" ++ basedir]) 
    cmpNames
   


groupVerilogs basedir (Program _ _ components) = do
    createDirectoryIfMissing True $ basedir ++ "/builds/.grouped"
    writeFile (basedir ++ "/builds/.grouped/build.grouped.v") ""
    -- copyVerilogs basedir cmpNames
    -- createProcess (shell $ "cat "++basedir++"/builds/.grouped/*.v > "++basedir++"/builds/.grouped/build.grouped.v") -- TODO: srsly?
    mapM_ (readAndAppend basedir) (cmpNames)
    appendFile (basedir ++ "/builds/.grouped/build.grouped.v") (dummyTop components)
    where
        cmpNames = map cmp_name components

readAndAppend :: FilePath -> String -> IO ()
readAndAppend basedir name = do
    verilogSrc <- readFile 
        $ basedir ++ "/builds/" ++ name ++ "/hdl/Main.topEntity/" ++ name ++ ".v" -- TODO: dit doe ik wel erg vaak handmatig
    appendFile (basedir ++ "/builds/.grouped/build.grouped.v") verilogSrc

copyVerilogs :: FilePath -> [String] -> IO ()
copyVerilogs basedir cmpNames = mapM_ (\name -> copyFile (source name) (dest name)) cmpNames
    where
        source cmp = basedir ++ "/builds/" ++ cmp ++ "/hdl/Main.topEntity/" ++ cmp ++ ".v"
        dest   cmp = basedir ++ "/builds/.grouped/"  ++ cmp ++ ".v"


dummyTop :: [Component] -> String
dummyTop components = moduleDef ++ inputstr ++ ",\n" ++ outputstr ++ "\n    );\n" ++ instantiationstr ++ "\nendmodule\n"
    where
        moduleDef = 
            "module top(\n\
            \    input clk,\n\
            \    input rst,\n\
            \    input en,\n"

        inputstr = intercalate ",\n" $
            map (verilogLine "input") $ 
            map (\cmp -> (cmp_name cmp, typeToBitwidth $ inputs $ cmp_isoStats cmp)) components
        -- outputstr = ""
        outputstr = intercalate ",\n" $
            map (verilogLine "output") $ 
            map (\cmp -> (cmp_name cmp, typeToBitwidth $ outputs $ cmp_isoStats cmp)) components
        instantiationstr = intercalate "\n" $
            map instantiationLine $ map cmp_name components

verilogLine :: String -> (String, Int) -> String
verilogLine prefix (name, bitwidth) = "    "++prefix++" ["++ show bitwidth ++":0] " ++ name ++ "_" ++ prefix 


instantiationLine :: String -> String
instantiationLine name = "    " ++ name ++ " " ++ name ++ "i(clk, rst, en, " ++ name ++ "_input, " ++ name ++ "_output);"


typeToBitwidth :: [ISOStat] -> Int
typeToBitwidth _ = 16 -- TODO: check clash implementatie

