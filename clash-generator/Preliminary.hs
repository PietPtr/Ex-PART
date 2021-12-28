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
makeComponentDirs :: [Component] -> IO ()
makeComponentDirs components = 
    mapM_ (\compName -> createDirectoryIfMissing True $ "builds/" ++ compName ++ "/") compNames
    where
        compNames = map cmp_type components


-- concat a list of haskell blocks
concatHaskell :: [HaskellDef] -> String
concatHaskell blocks 
    = concat 
    $ map unlines 
    -- $ map (map trim) 
    $ map lines 
    $ map (\(HaskellDef code) -> code) blocks 

-- create a Definitions.hs with all comb blocks and a module def and imports
genDefs :: TopData -> String
genDefs (TopData combinatories _) = 
    "{-# LANGUAGE NumericUnderscores #-}\nmodule Definitions where\nimport Clash.Prelude\nimport qualified Data.List as L\n\n" ++
    concatHaskell combinatories
genDefs (NotTop) = error "Preliminary.hs: Something went wrong during elaboration, the top system does not have top-data."

writeDefs :: TopData -> IO ()
writeDefs topdata = writeFile ("Definitions.hs") $ genDefs topdata

-- chain the above functions to achieve actions described
doPreliminaryProcessing :: System -> IO ()
doPreliminaryProcessing system = do
    makeBuild
    makeComponentDirs (top_cmps $ sys_topdata system)
    writeDefs (sys_topdata system)