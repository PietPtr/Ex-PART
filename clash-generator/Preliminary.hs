module Preliminary where

import Types
import Data.List
import Data.Char (isSpace)
import System.Directory

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- create builds folder
makeBuild :: FilePath -> IO ()
makeBuild basedir = createDirectoryIfMissing True $ basedir ++ "/builds/"

-- create folder for every unique inst of component
makeComponentDirs :: FilePath -> Program -> IO ()
makeComponentDirs basedir (Program _ _ components) = 
    mapM_ (\compName -> createDirectoryIfMissing True $ basedir ++ "/builds/" ++ compName ++ "/") compNames
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

writeDefs :: FilePath -> Program -> IO ()
writeDefs basedir program = writeFile (basedir ++ "/Definitions.hs") $ genDefs program

-- chain the above functions to achieve actions described
doPreliminaryProcessing :: FilePath -> Program -> IO ()
doPreliminaryProcessing basedir program = do
    makeBuild basedir
    makeComponentDirs basedir program
    writeDefs basedir program