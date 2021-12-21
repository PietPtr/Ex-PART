

# Program Structure

## Compiler (`compiler`)

## Parser (`parser`)

## Elaboration (`elaboration`)

## Clash Generator (`clash-generator`)

## JSON Builder (`json-builder`)

## Yosys (`yosys`)

## Nextpnr (`nextpnr`)


# Feature Implementations

Below the same feature list as in the [programming manual](programming.md) is shown. Here a brief explanation of the implementation of the feature is provided, including where to find the code.

## Comments

## `.expc` file

## `haskell` block

## Component definition

### I/O ports and state

### Transition statements

## `.expi` file

## Coordinates and Sizes

## System Definitions

### I/O ports

### Subsystems

## Component Instantiation

## Port connection

## Constant Drivers

## Repeat statement

## Chain statement

## Multiconnections

## Unplaced Systems

## System Instantiation



# Error List

## Errors

- `clash-generator/ComponentConversion.hs:82:` A state is not a port.
- `clash-generator/Flattener.hs:131:` No connection specified for element
- `clash-generator/Flattener.hs:151:` No connection specified for io statement 
- `clash-generator/Preliminary.hs:38:` Something went wrong during elaboration, the top system does not have top-data.
- `compiler/Compiler.hs:91:` No components in expc file...
- `compiler/Compiler.hs:100:` How can there be several components with the same name?
- `elaboration/ElaborateConnection.hs:9:` Cannot connect ports $from -> $to ++
- `elaboration/ElaborateConnection.hs:25:` Cannot find port with name 
- `elaboration/ElaborateConnection.hs:27:` Found several ports with name 
- `elaboration/ElaborateConnection.hs:40:` Cannot find element with name 
- `elaboration/ElaborateConnection.hs:42:` Found several components with name 
- `elaboration/Elaboration.hs:76:` Cannot find system $name in this scope. $(map systr_name systrees)
- `elaboration/Multiconnection.hs:8:` Cannot connect differing amount of ports: $from' -> $to'
- `elaboration/Multiconnection.hs:20:` Cannot find repetition with name `" ++ repName ++
- `elaboration/Repetition.hs:34:` Missing option `chain_in` in chain statement
- `elaboration/Repetition.hs:37:` Missing option `chain_out` in chain statement
- `elaboration/Repetition.hs:42:` Missing option `component` in repetition statement.
- `elaboration/Repetition.hs:45:` Missing option `amount` in a repetition statement.
- `elaboration/Repetition.hs:48:` Missing option `layout` in a repetition statement.
- `elaboration/Repetition.hs:119:` Cannot find element $elemName in source files.
- `elaboration/Repetition.hs:172:` Unknown layout procedure.
- `json-builder/JSONBuilder.hs:21:` Top-level coordinates must be constants.
- `json-builder/JSONBuilder.hs:32:` expi file contains a cyclic coordinate definition, cannot generate location JSON.
- `json-builder/Locations.hs:101:` Coordinate reduction found non-constant value ($expr")
- `json-builder/Locations.hs:154:` Could not find ID $id in provided list.
- `nextpnr/Nextpnr.hs:28:` nextpnr terminated with code $code
- `parser/Types.hs:149:` Cannot find bitwidth of type $type
- `parser/Types.hs:102:` Invalid ISOStatement for bitwidth (state not implemented)
  - In the current implementation of Ex-PART, the bitwidth of states is irrelevant. Therefore no conversion to bitwidth from states is implemented.
- `yosys/Postprocessing.hs:204:` Could not find driver $cid in $(sys_connections system)
- `yosys/Postprocessing.hs:331:` cannot find net for cid in netmap:n" ++
- `yosys/Postprocessing.hs:333:` No net found, something is disconnected...n" ++
- `yosys/Preprocessing.hs:57:` No state should have been seen here.
- `yosys/Preprocessing.hs:63:` Found zero-output component.
- `yosys/Preprocessing.hs:68:` Not an input: $x
- `yosys/Preprocessing.hs:74:` Not an output: $x
- `yosys/Yosys.hs:54:` Clash terminated with code $code
- `yosys/Yosys.hs:78:` Clash terminated with code $code
- `yosys/Yosys.hs:97:` Clash terminated with code $code
- `yosys/Yosys.hs:138:` Yosys terminated with code $code