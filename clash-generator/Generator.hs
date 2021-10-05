module Generator where

import Types
import Preliminary (doPreliminaryProcessing)
import ComponentConversion (toClash)

generateComponent :: Component -> IO ()
generateComponent cmp = writeFile 
    ("builds/" ++ name ++ "/Synth_" ++ name ++ ".hs") 
    (toClash cmp) 
    where
        name = cmp_name cmp

generateClash :: Program -> IO ()
generateClash program@(Program _ _ components) = do
    doPreliminaryProcessing program
    mapM_ (generateComponent) components
