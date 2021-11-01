module Repetition where

import Types
import Chain
import Repeat
import Multiconnection

-- Ensures all repetitions and multiconnections are unrolled into instances and connections
-- TODO: split up data types such that this guarantee is type-checkable?
unroll :: Program -> System -> System
unroll (Program _ _ cmps) system = system {
        sys_instances = sys_instances system ++ unrolledInstances,
        sys_connections = sys_connections system ++ unrolledConnections
    }
    where
        -- TODO: change unrollRepeat to generic unroll for both repeat and chain
        unrolledInstances = concat $ map (unrollRepeat cmps) (sys_repetitions system)
        unrolledConnections = concat $ 
            map (unrollMulticonn $ sys_repetitions system) (sys_multicons system)