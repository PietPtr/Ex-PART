module Preprocessing where

import System.Directory
import System.Process
import Data.List

import Types

-- assumes that every folder in builds except ".grouped" needs to be compiled
-- compileToVerilog :: FilePath -> IO ()


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

        allCompInputs = concat $ 
            map (\cmp -> map (\i -> (cmp_name cmp, i)) (inputs $ cmp_isoStats cmp)) components
        inputstr = intercalate ",\n" $
            map (verilogLine "input") allCompInputs
            
        allCompOutputs = concat $
            map (\cmp -> map (\o -> (cmp_name cmp, o)) (outputs $ cmp_isoStats cmp)) components
        outputstr = intercalate ",\n" $
            map (verilogLine "output") allCompOutputs

        instantiationstr = intercalate "\n" $
            map instantiationLine components

verilogLine :: String -> (String, ISOStat) -> String
verilogLine prefix (cmp, stat) = 
    "    "++prefix++" ["++ show (bitwidth - 1) ++":0] " ++ cmp ++ "_" ++ prefix ++ "_" ++ name
    where
        bitwidth = isoStatToBitwidth stat
        name = case stat of
            (SInput name _) -> name
            (SOutput name _) -> name


instantiationLine' :: String -> String
instantiationLine' name = "    " ++ name ++ " " ++ name ++ "i(clk, rst, en, " ++ name ++ "_input, " ++ name ++ "_output);"

instantiationLine :: Component -> String
instantiationLine cmp = "    " ++ name ++ " " ++ name ++ "i(clk, rst, en, " ++
    inputNames ++ ", " ++ outputNames ++ ");"
    where
        inps = inputs $ cmp_isoStats cmp
        inputNames = intercalate ", " $ map inputName inps
        inputName (SInput inp_name _) = name ++ "_input_" ++ inp_name


        outs = outputs $ cmp_isoStats cmp
        outputNames = intercalate ", " $ map outputName outs
        outputName (SOutput out_name _) = name ++ "_output_" ++ out_name

        name = cmp_name cmp


            
