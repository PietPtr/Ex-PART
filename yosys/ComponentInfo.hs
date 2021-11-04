{-# LANGUAGE OverloadedStrings #-} 

module ComponentInfo where

import Data.Map (Map)
-- import Data.HashMap.Base (HashMap, (!), keys, filterWithKey)
-- import qualified Data.HashMap.Base as HM -- TODO: dit is allemaal totaal niet portable of zo
import qualified Data.Map as Map
import Data.Aeson
import System.Directory
import Debug.Trace
import Data.Text hiding (map)

import Types

data CInfo = CInfo {
        inf_componentName :: String,
        inf_cellAmounts :: Map String Int
    } deriving Show


instance ToJSON CInfo where
    toJSON cinfo = object [ pack (inf_componentName cinfo) .= 
        toJSON (inf_cellAmounts cinfo) ]

-- loadSynthesized :: IO (Maybe Value)
-- loadSynthesized = decodeFileStrict "base.json"

-- -- TODO: Aeson heeft hele mooie parser-based manieren om dit te fixen, ipv hacken met hashmaps en filters
-- extractCInfo :: Value -> Program -> [CInfo]
-- extractCInfo (Object obj) program = HM.elems $ HM.mapWithKey findCInfo currentComponents
--     where
--         (Object modules) = obj ! "modules"
--         currentComponents = filterWithKey nameIn modules
--         nameIn k mod = k `elem` componentNames
--         componentNames = map (pack . cmp_name) (prg_cmps program)

-- findCInfo :: Text -> Value -> CInfo
-- findCInfo name (Object obj) = CInfo 
--     { inf_componentName = unpack name
--     , inf_cellAmounts = counts }
--     where
--         (Object cells) = obj ! "cells"
--         -- TODO: this is the ugliest haskell i have ever seen, please just learn how to use Aeson well.
--         counts = Map.fromList $ map (\(String a, num) -> (unpack a, num)) $ Map.toList $ countOccurences $
--             map snd $ HM.toList $ HM.map (\(Object c) -> c ! "type") cells


-- countOccurences :: Ord a => [a] -> Map a Int
-- countOccurences xs = count xs Map.empty
--     where
--         count [] countMap = countMap
--         count (x:xs) countMap = Map.alter f x (count xs countMap)
--         f Nothing = Just 1
--         f (Just c) = Just (c + 1)

