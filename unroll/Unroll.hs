module Unroll where

import Types
import Chain
import Repeat
import Multiconnection
-- import Constant

-- Ensures all repetitions and multiconnections are unrolled into instances and connections
-- TODO: split up data types such that this guarantee is type-checkable?
unroll :: Program -> System -> System
unroll (Program _ _ cmps) system = system {
        sys_instances = 
            sys_instances system ++ 
            unrolledInstances,
        sys_connections = 
            sys_connections system ++ 
            unrolledConnections ++ 
            allChainConnections
    }
    where
        -- TODO: change unrollRepeat to generic unroll for both repeat and chain
        unrolledInstances = concat $ map (\r -> (unrollRepetition r) cmps) (sys_repetitions system)
        unrolledConnections = concat $ 
            map (unrollMulticonn $ sys_repetitions system) (sys_multicons system)
        allChainConnections = concat $
            map chainConnections ([ x | x@(Chain {}) <- sys_repetitions system])


unrollRepetition :: Repetition -> ([Component] -> [Instance])
unrollRepetition rep = case rep of
    Chain {} -> (\cs -> unrollChain cs rep)
    Repeat {} -> (\cs -> unrollRepeat cs rep)

