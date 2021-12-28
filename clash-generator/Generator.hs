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
        name = cmp_type cmp

generateClash :: System -> IO ()
generateClash top = do
    doPreliminaryProcessing top
    mapM_ (generateComponent) (top_cmps $ sys_topdata top)


flatten :: Bool -> System -> IO ()
flatten inline system = do
    writeFile ("Clash.hs") (F.flatten inline system)
