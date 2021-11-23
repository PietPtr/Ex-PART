module Generator where

import Types
import Preliminary (doPreliminaryProcessing)
import ComponentConversion (toClash)
import qualified Flattener as F

generateComponent :: Component -> IO ()
generateComponent cmp = writeFile 
    ("builds/" ++ name ++ "/Synth_" ++ name ++ ".hs") 
    (toClash cmp) 
    where
        name = cmp_name cmp

generateClash :: System -> IO ()
generateClash system = do
    doPreliminaryProcessing system
    mapM_ (generateComponent) (sys_components system)

flatten :: System -> IO ()
flatten system = do
    writeFile ("Clash.hs") (F.flatten system)