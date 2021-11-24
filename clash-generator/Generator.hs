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

-- TODO (lowprio): Een het idee is een system met topdata top te noemen, twee opties:
--      1: maak een apart TopSystem type
--      2: of noem het overal top waar het een topentity is (doe optie 1 eerst, daarna 2 doorvoeren?)
--      Ik vind het ook wel prima zoals het nu is
generateClash :: System -> IO ()
generateClash top = do
    doPreliminaryProcessing top
    mapM_ (generateComponent) (top_cmps $ sys_topdata top)

flatten :: System -> IO ()
flatten system = do
    writeFile ("Clash.hs") (F.flatten system)