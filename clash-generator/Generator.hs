module Generator where

import Types
import Preliminary (doPreliminaryProcessing)
import ComponentConversion (toClash)
import qualified Flattener as F
import Data.Char

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

flatten :: Program -> System -> IO ()
flatten expc expi = do
    writeFile ((toUpper f):topEntity ++ "_clash.hs") (F.flatten expc expi)
    where
        (f:topEntity) = sys_id expi