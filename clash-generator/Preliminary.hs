module Preliminary where

import Types
import Data.List
import Data.Char (isSpace)
import System.Directory

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- create builds folder
makeBuild :: IO ()
makeBuild = createDirectoryIfMissing True "builds/"

-- create folder for every unique inst of component
makeComponentDirs :: Program -> IO ()
makeComponentDirs (Program _ _ components) = 
    mapM_ (\compName -> createDirectoryIfMissing True $ "builds/" ++ compName ++ "/") compNames
    where
        compNames = map cmp_name components

-- find every unique inst of component
uniqueComponents :: Program -> [Component]
uniqueComponents (Program _ _ comps) = comps -- Version Two: support arguments

-- concat a list of combinatory blocks
concatCombinatory :: [Combinatory] -> String
concatCombinatory blocks 
    = concat 
    $ map unlines 
    $ map (map trim) 
    $ map lines 
    $ map (\(Combinatory code) -> code) blocks 

-- create a Definitions.hs with all comb blocks and a module def and imports
genDefs :: Program -> String
genDefs (Program haskellDefs combinatories _) = 
    "module Definitions where\nimport Clash.Prelude\n\n" ++
    concat (intersperse "\n" haskellDefs) ++ "\n\n" ++
    concatCombinatory combinatories

writeDefs :: Program -> IO ()
writeDefs program = writeFile ("Definitions.hs") $ genDefs program

-- chain the above functions to achieve actions described
doPreliminaryProcessing :: Program -> IO ()
doPreliminaryProcessing program = do
    makeBuild
    makeComponentDirs program
    writeDefs program