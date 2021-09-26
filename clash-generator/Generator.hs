module Generator where

import Types
import Preliminary (doPreliminaryProcessing)
import ComponentConversion (toClash)

generateComponent :: FilePath -> Component -> IO ()
generateComponent basedir cmp = writeFile 
    (basedir ++ "/builds/" ++ name ++ "/Synth_" ++ name ++ ".hs") 
    (toClash cmp) 
    where
        name = cmp_name cmp

generateClash :: FilePath -> Program -> IO ()
generateClash basedir program@(Program _ _ components) = do
    doPreliminaryProcessing basedir program
    mapM_ (generateComponent basedir) components
