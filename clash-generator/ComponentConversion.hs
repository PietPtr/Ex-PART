module ComponentConversion where

import Types
import Data.List

-- X fill in arguments
-- ISSUE #19: add constant component arguments (generics)

-- create Haskell type signature from ISO
haskellifyISO :: (ISOStat -> String) -> [ISOStat] -> String
haskellifyISO func stats = "(" ++ (concat $ intersperse ", " $ map func stats) ++ ")"

createTypeSignature :: Component -> String
createTypeSignature (Component name _ isoStats _) = 
    name ++ " :: " ++ stateType isoStats ++ " -> " ++ 
    inputType isoStats ++ " -> (" ++ stateType isoStats ++ 
    ", " ++ outputType isoStats ++ ")"

inputType :: [ISOStat] -> String
inputType isoStats = haskellifyISO (\(SInput _ t) -> t) (inputs isoStats)
stateType :: [ISOStat] -> String
stateType isoStats = haskellifyISO (\(SState _ _ t) -> t) (states isoStats)
outputType :: [ISOStat] -> String
outputType isoStats = haskellifyISO (\(SOutput _ t) -> t) (outputs isoStats)

-- create skeleton Haskell def
createEquation :: Component -> String
createEquation (Component name _ isoStats _) = 
    name ++ " " ++ stateNames ++ " " ++ inputNames ++ " = (" ++ stateNames' ++ ", " ++ outputNames ++ ")"
    where
        inputNames = haskellifyISO (\(SInput name _) -> name) (inputs isoStats)
        stateNames = haskellifyISO (\(SState name _ _) -> name) (states isoStats)
        outputNames = haskellifyISO (\(SOutput name _) -> name) (outputs isoStats)
        stateNames' = haskellifyISO (\(SState name _ _) -> name ++ "\'") (states isoStats)

-- fill in where clause
-- If some unexpected stuff is hapenning in component definitions, try to keep indentation four spaces, or look at the examples for inspiration...
createWhereClause :: Component -> String
createWhereClause (Component _ _ _ whereBlock) = "    where\n    " ++ 
    (unlines $ map ("    " ++) $ lines whereBlock)

-- prepare for synthesis
createMealy :: Component -> String
createMealy (Component name _ isoStats _) = concat $ intersperse "\n" 
    [mealyType, mealyDef, ""]
    where
        mealyType = name ++ "M :: HiddenClockResetEnable dom =>\n    Signal dom " ++ inputType isoStats ++
            " -> Signal dom " ++ outputType isoStats

        mealyDef = name ++ "M = mealy " ++ name ++ " (" ++ initialStates ++ ")"
        initialStates = (concat $ intersperse ", " $
            map (\(SState _ init _) -> haskellifyConstExpr init) (states isoStats))

createSynthesizableComponent :: Component -> String
createSynthesizableComponent cmp@(Component name _ isoStats _) = intercalate "\n" $
    [createMealy cmp, createSynthesizable isoStats name True]

createSynthesizable :: [ISOStat] -> String -> Bool -> String
createSynthesizable isoStats name isComponent = concat $ intersperse "\n" 
    [annotation, "", topEntity]
    where        
        annotation = 
            "{-# ANN topEntity\n\
            \  (Synthesize\n\
            \    { t_name = \""++name++"\"\n\
            \    , t_inputs = [ PortName \"clk\", PortName \"rst\", PortName \"en\", "++ inPortProduct ++" ]\n\
            \    , t_output = "++ outPortProduct ++"\n\
            \    }) #-}"

        inPortProduct = generatePort inputs
        outPortProduct = generatePort outputs
       
        generatePort :: ([ISOStat] -> [ISOStat]) -> String
        generatePort f = 
            if length (f isoStats) == 1 
                then toPortName (head $ f isoStats)
                else "PortProduct \"\" [" ++ concat (intersperse ", " $ map toPortName $ f isoStats) ++ "]"

        toPortName :: ISOStat -> String
        toPortName (SInput name _) = "PortName \""++name++"\""
        toPortName (SOutput name _) = "PortName \""++name++"\""
        toPortName _ = error "ComponentConversion.hs: A state is not a port."

        topEntity =
            "topEntity\n\
            \    :: Clock System\n\
            \    -> Reset System\n\
            \    -> Enable System\n\
            \    -> Signal System "++inputType isoStats++"\n\
            \    -> Signal System "++outputType isoStats++"\n\
            \topEntity = exposeClockResetEnable "++name++(if isComponent then "M" else "")

haskellifyConstExpr :: ConstExpr -> String
haskellifyConstExpr expr = case expr of
    (Constant n) -> show n
    (HaskellData n) -> n

-- add imports and combine everything
toClash' :: Component -> String
toClash' cmp = concat $ intersperse "\n" $
    [ createTypeSignature cmp
    , createEquation cmp
    , createWhereClause cmp
    , createSynthesizableComponent cmp ]

toClash :: Component -> String
toClash cmp = imports ++ toClash' cmp
    where
        imports = "import Clash.Prelude\nimport Definitions\n\n"