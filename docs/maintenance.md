

# Program Structure

## Compiler (`compiler`)

## Parser (`parser`)

## Elaboration (`elaboration`)

## Clash Generator (`clash-generator`)

## JSON Builder (`json-builder`)

## Yosys (`yosys`)

## Nextpnr (`nextpnr`)


# Error Messages

Ex-PART's error system is _very_ simple: it uses haskell's `error :: String -> a` function whenever anything unexpected happens. Occasionally Ex-PART will dump extra output in the error message as well. This list aims to go through all error messages Ex-PART may throw and provide a short explanation on why this error may be thrown and what can be done to fix it.

