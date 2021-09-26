module ComponentConversion where

import Types
import Data.List

-- X step 1: fill in arguments
-- Version Two: implement this

inputs :: [ISOStat] -> [ISOStat]
inputs [] = []
inputs (stat:stats) = case stat of
    (SInput _ _) -> stat : inputs stats
    _ -> inputs stats

states :: [ISOStat] -> [ISOStat]
states [] = []
states (stat:stats) = case stat of
    (SState _ _ _) -> stat : states stats
    _ -> states stats

outputs :: [ISOStat] -> [ISOStat]
outputs [] = []
outputs (stat:stats) = case stat of
    (SOutput _ _) -> stat : outputs stats
    _ -> outputs stats

-- step 2: create Haskell type signature from ISO
haskellifyISO :: (ISOStat -> String) -> [ISOStat] -> String
haskellifyISO func stats = "(" ++ (concat $ intersperse ", " $ map func stats) ++ ")"

createTypeSignature :: Component -> String
createTypeSignature (Component name _ isoStats _) = 
    name ++ " :: " ++ stateType isoStats ++ " -> " ++ 
    inputType isoStats ++ " -> (" ++ stateType isoStats ++ 
    ", " ++ outputType isoStats ++ ")"

inputType isoStats = haskellifyISO (\(SInput _ t) -> t) (inputs isoStats)
stateType isoStats = haskellifyISO (\(SState _ _ t) -> t) (states isoStats)
outputType isoStats = haskellifyISO (\(SOutput _ t) -> t) (outputs isoStats)

-- step 3: create skeleton Haskell def
createEquation :: Component -> String
createEquation (Component name _ isoStats _) = 
    name ++ " " ++ stateNames ++ " " ++ inputNames ++ " = (" ++ stateNames' ++ ", " ++ outputNames ++ ")"
    where
        inputNames = haskellifyISO (\(SInput name _) -> name) (inputs isoStats)
        stateNames = haskellifyISO (\(SState name _ _) -> name) (states isoStats)
        outputNames = haskellifyISO (\(SOutput name _) -> name) (outputs isoStats)
        stateNames' = haskellifyISO (\(SState name _ _) -> name ++ "\'") (states isoStats)

-- step 4: fill in where clause
-- BUG: dit soort trucedozen gaan echt weirde errors geven bij eindgebruikers die dingen anders doen dan ik
createWhereClause :: Component -> String
createWhereClause (Component _ _ _ whereBlock) = "    where\n    " ++ 
    (unlines $ map ("    " ++) $ lines whereBlock)

-- step 5: prepare for synthesis
createSynthesizable :: Component -> String
createSynthesizable (Component name _ isoStats _) = concat $ intersperse "\n" 
    [mealyType, mealyDef, "", annotation, "", topEntity]
    where
        mealyType = name ++ "M :: HiddenClockResetEnable dom =>\n    Signal dom " ++ inputType isoStats ++
            " -> Signal dom " ++ outputType isoStats

        mealyDef = name ++ "M = mealy " ++ name ++ " (" ++ initialStates ++ ")"
        initialStates = (concat $ intersperse ", " $
            map (\(SState _ init _) -> haskellifyConstExpr init) (states isoStats))
        
        annotation = 
            "{-# ANN topEntity\n\
            \  (Synthesize\n\
            \    { t_name = \""++name++"\"\n\
            \    , t_inputs = [ PortName \"clk\", PortName \"rst\", PortName \"en\", "++ inPortProduct ++" ]\n\
            \    , t_output = "++ outPortProduct ++"\n\
            \    }) #-}"
        inPortProduct = "PortProduct \"\" [" ++ concat (intersperse ", " $ map toPortName $ inputs isoStats) ++ "]"
        outPortProduct = "PortProduct \"\" [" ++ concat (intersperse ", " $ map toPortName $ outputs isoStats) ++ "]"

        toPortName :: ISOStat -> String
        toPortName (SInput name _) = "PortName \""++name++"\""
        toPortName (SOutput name _) = "PortName \""++name++"\""

        topEntity =
            "topEntity\n\
            \    :: Clock System\n\
            \    -> Reset System\n\
            \    -> Enable System\n\
            \    -> Signal System "++inputType isoStats++"\n\
            \    -> Signal System "++outputType isoStats++"\n\
            \topEntity = exposeClockResetEnable "++name++"M"

haskellifyConstExpr :: ConstExpr -> String
haskellifyConstExpr (Constant n) = show n

-- step 6: add imports and combine everything
toClash :: Component -> String
toClash cmp = concat $ intersperse "\n" $
    [ imports, createTypeSignature cmp, createEquation cmp, createWhereClause cmp, createSynthesizable cmp ]
    where
        imports = "import Clash.Prelude\nimport Definitions\n"