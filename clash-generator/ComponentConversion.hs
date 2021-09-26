module ComponentConversion where

testprog = Program 
    ["type Value = Unsigned 8"] 
    [Combinatory "\n    (>>>) :: Bits a => a -> Int -> a\n    (>>>) = shiftR\n    \n    (<<<) :: Bits a => a -> Int -> a\n    (<<<) = shiftL\n"] 
    [Component "control" [] 
        [SInput "next_val" "Value",SInput "set_val" "Maybe Value",SState "last_val" (Constant 0) "Value",SOutput "result_value" "Value"] 
        "last_val' = case set_val of\n        Just new_value -> new_value\n        Nothing -> next_val\n\n    result_value = last_val\n",
     Component "merger" [] 
        [SInput "vo" "Maybe Value",SInput "ve" "Maybe Value",SOutput "res" "Value"] 
        "res = case vo of\n        Just v -> v\n        Nothing -> case ve of\n            Just v -> v\n            Nothing -> 0\n",
     Component "onOdd" [] 
        [SInput "val" "Maybe Value",SOutput "res" "Maybe Value"] 
        "res = case value of\n        Just v -> Just $ (v <<< 1 + v) + 1\n        Nothing -> Nothing\n",
     Component "onEven" [] 
        [SInput "val" "Maybe Value",SOutput "res" "Maybe Value"] 
        "res = case value of\n        Just v -> Just $ v >>> 1\n        Nothing -> Nothing\n",
     Component "router" [] 
        [SInput "val" "Value",SOutput "odd" "Maybe Value",SOutput "even" "Maybe Value"] 
        "odd  = if testBit val 0 then Nothing else Just val\n    even = if testBit val 0 then Just val else Nothing\n"
    ]

-- X step 1: fill in arguments

-- step 2: create Haskell type signature from IOS

-- step 3: create skeleton Haskell def

-- step 4: fill in where clause

-- step 5: prepare for synthesis

-- step 6 add imports