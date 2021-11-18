module Generics where

import Types

{-
Given a system, find all instances where ins_args is non-empty.
For these instances, implement the generic_component given the arguments.
    `implement': re-parse the where statement for #identifier and build a list of
        Either String Identifier, and join that list based on the bindings given in
        ins_args for each identifier named in cmp_args to generate a new where-clause.
The new components shall be added to a list of new components to be returned and used in
    the rest of the process, while making sure they stay unique to _all_ given components.
At the end of the call we have:
    - new components, the implementations of generic components.
    - modified instances: arguments are empty and the component is replaced by the 
        newly implemneted generic component
For the recursive call for other systems, first add the new components to the component list
    then recurse with that list. This way components stay globally unique.

When the generic unroll step is completely finished, every instance will have ins_args empty,
and a component implemented as specified.
-}

