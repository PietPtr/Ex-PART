module Preprocessing where

import System.Process
import Data.List

import Types


clashProcesses :: [String] -> [CreateProcess]
clashProcesses cmpNames = map 
    (\name -> (proc "clash" [
        "builds/"++name++"/Synth_" ++ name ++ ".hs", 
        "--verilog", 
        "-outputdir .hs", 
        "-fclash-hdldir builds/"++name++"/hdl"
        ]){std_out=CreatePipe, std_err=CreatePipe}) 
    cmpNames
   

readAndAppend :: String -> IO ()
readAndAppend name = do
    verilogSrc <- readFile 
        $ "builds/" ++ name ++ "/hdl/Main.topEntity/" ++ name ++ ".v" -- TODO (lowprio): dit doe ik wel erg vaak handmatig
    appendFile ("builds/.grouped/build.grouped.v") verilogSrc


dummyTop :: [Component] -> String
dummyTop components = moduleDef ++ inputstr ++ ",\n" ++ outputstr ++ "\n    );\n" ++ instantiationstr ++ "\nendmodule\n"
    where
        moduleDef = 
            "module $top(\n\
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
            _ -> error "Preprocessing.hs: No state should have been seen here."

instantiationLine :: Component -> String
instantiationLine cmp = if (length outs > 0)
    then "    " ++ name ++ " " ++ name ++ "i(clk, rst, en, " ++
        inputStr ++ outputNames ++ ");"
    else error "Preprocessing.hs: Found zero-output component."
    where
        inps = inputs $ cmp_isoStats cmp
        inputNames = intercalate ", " $ map inputName inps
        inputName (SInput inp_name _) = name ++ "_input_" ++ inp_name
        inputName x = error $ "Preprocessing.hs: Not an input: " ++ show x
        inputStr = if length inps == 0 then "" else inputNames ++ ", "

        outs = outputs $ cmp_isoStats cmp
        outputNames = intercalate ", " $ map outputName outs
        outputName (SOutput out_name _) = name ++ "_output_" ++ out_name
        outputName x = error $ "Preprocessing.hs: Not an output: " ++ show x

        name = cmp_name cmp


            
