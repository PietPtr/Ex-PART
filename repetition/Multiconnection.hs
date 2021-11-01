module Multiconnection where

import Types

unrollMulticonn :: [Repetition] -> MultiConnection -> [Connection]
unrollMulticonn reps (MultiConn from to) = if length from' == length to'
    then zipWith Connection from' to'
    else error $ "Cannot connect differing amount of ports: " ++ show from' ++ " -> " ++ show to'
    where
        from' = unrollMCID reps from
        to' = unrollMCID reps to

unrollMCID :: [Repetition] -> MCID -> [CID] -- if resulting cid lists don't match higher up, error
unrollMCID reps (MCID repName portName range) = case range of
    All -> map makeCID [0..(rep_amount rep - 1)]
    Range start end -> map makeCID [start..end]
    where
        
        rep = case filter (\r -> repetitionName r == repName) reps of
            (x:_) -> x
            [] -> error $ "Cannot find repetition with name `" ++ repName ++ 
                "` for multiconnection " ++ repName ++ ":" ++ portName ++ "."

        makeCID i = CID (rep_name rep ++ "_" ++ show i) portName


repetitionName :: Repetition -> String
repetitionName rep = case rep of
    c@(Chain) -> error "not implemented."
    r@(Repeat _ _ _ _ _) -> rep_name r

{-
CID "enablers_0" "enable",
CID "enablers_1" "enable",
CID "enablers_2" "enable",
CID "enablers_3" "enable",
CID "enablers_4" "enable",
CID "enablers_5" "enable"] -> 
CID "slowCounters_0" "enable",
CID "slowCounters_1" "enable",
CID "slowCounters_2" "enable",
CID "slowCounters_3" "enable",
CID "slowCounters_4" "enable"]

-}