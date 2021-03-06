- [Introduction](#introduction)
- [Types](#types)
  - [`LayoutExpr`](#layoutexpr)
  - [`ISOStat` and `IOStat`](#isostat-and-iostat)
  - [`Component`](#component)
  - [`Instance`](#instance)
  - [`ExpcDesign`](#expcdesign)
  - [`Design`](#design)
  - [`SystemTree`](#systemtree)
  - [`System`](#system)
  - [`Element`](#element)
  - [`Connection`(`'`) and `CID`](#connection-and-cid)
  - [`RawRepetition`](#rawrepetition)
  - [`Repetition`](#repetition)
  - [`MultiConnection` and `MCID`](#multiconnection-and-mcid)
  - [`ConstantDriver`](#constantdriver)
- [Program Structure](#program-structure)
  - [Compiler (`compiler/`)](#compiler-compiler)
    - [`Compiler.hs`](#compilerhs)
    - [`Flows.hs`](#flowshs)
    - [`Steps.hs`](#stepshs)
  - [Parser (`parser/`)](#parser-parser)
    - [`Parse_shared.hs`](#parse_sharedhs)
    - [`Parse_expc.hs`](#parse_expchs)
    - [`Parse_expi.hs`](#parse_expihs)
    - [`Parser.hs`](#parserhs)
    - [`Types.hs`](#typeshs)
  - [Elaboration (`elaboration/`)](#elaboration-elaboration)
    - [`Elaboration.hs`](#elaborationhs)
    - [`Repetition.hs`](#repetitionhs)
    - [`ElaborateConnection.hs`](#elaborateconnectionhs)
    - [`Multiconnection.hs`](#multiconnectionhs)
  - [JSON Builder (`json-builder/`)](#json-builder-json-builder)
    - [`Locations.hs`](#locationshs)
    - [`JSONBuilder.hs`](#jsonbuilderhs)
  - [Clash Generator (`clash-generator/`)](#clash-generator-clash-generator)
    - [`Generator.hs`](#generatorhs)
    - [`Preliminary.hs`](#preliminaryhs)
    - [`ComponentConversion.hs`](#componentconversionhs)
    - [`Flattener.hs`](#flattenerhs)
  - [Yosys (`yosys/`)](#yosys-yosys)
    - [`Preprocessing.hs`](#preprocessinghs)
    - [`Yosys.hs`](#yosyshs)
    - [`Postprocessing.hs`](#postprocessinghs)
    - [Other Files](#other-files)
  - [Nextpnr (`nextpnr/`)](#nextpnr-nextpnr)
    - [`Nextpnr.hs`](#nextpnrhs)
    - [`constrainer.py`](#constrainerpy)
  - [Visualizer (`visualizer/`)](#visualizer-visualizer)
    - [`color.py`](#colorpy)
    - [`init.py`](#initpy)
    - [`files.py`](#filespy)
    - [`iodb.json`, `blinky.lpf` and `parse_iodb.py`](#iodbjson-blinkylpf-and-parse_iodbpy)
    - [`iodata.csv` and `tiledata.csv`](#iodatacsv-and-tiledatacsv)
    - [`grid.py`](#gridpy)
    - [`slice.py`](#slicepy)
    - [`routing.py`](#routingpy)
    - [`legend.py`](#legendpy)
    - [`connections.py`](#connectionspy)
    - [`systems.py`](#systemspy)
    - [`main.py`](#mainpy)
- [Feature Implementations](#feature-implementations)
  - [Comments](#comments)
  - [`haskell` block](#haskell-block)
  - [Component definition](#component-definition)
  - [Coordinates and Sizes](#coordinates-and-sizes)
  - [System Definitions](#system-definitions)
  - [Component Instantiation](#component-instantiation)
  - [Port connection](#port-connection)
  - [Constant Drivers](#constant-drivers)
  - [Repeat and Chain statement](#repeat-and-chain-statement)
  - [Multiconnections](#multiconnections)
  - [Unplaced Systems](#unplaced-systems)
  - [System Instantiation](#system-instantiation)
- [Error List](#error-list)
  - [Errors](#errors)

# Introduction

This is the _maintenance manual_ for Ex-PART. If you are planning on adding features or fixing bugs, you should read the relevant portions of this manual first. Information in this manual can also be of service when designing hardware and encountering unexpected or incorrect behaviour (e.g. in the [error section](#error-list)).

# Types

In `parser/Types.hs` types that are used in the entire project are defined. The idea behind most of them and their uses are outlined here. See also issue [#18](https://github.com/PietPtr/Ex-PART/issues/18).

## `LayoutExpr`
Recursive data type for containing size and coordinate expressions as they are defined by the designer. This data type is necessary as it is impossible to immediately _evaluate_ expressions during parsing. With this data type expressions can be stored until they actually can be evaluated, after elaboration. There is an instance of the `Pretty` typeclass to pretty-print expressions to make them easier to debug.

## `ISOStat` and `IOStat`

`ISOStat` is an `I`nput, `S`tate, or `O`utput statement. These contain the information defined in the statements at the start of a component definition. `IOStat` is an `I`nput or `O`utput statement, so the statements at the start of (sub-)system definitions. These types basically contain the same information, and perhaps merging them may be better, as very often it is necessary to do ad-hoc conversions between them. See also issue [#21](https://github.com/PietPtr/Ex-PART/issues/21).

## `Component`
Stores the information in a component definition: a list of `ISOStat`s, the name of the component type, and the where block. Currently there still is a list of constants arguments as well, but that is always an empty list. See also issue [#19](https://github.com/PietPtr/Ex-PART/issues/19).

## `Instance`
In instance is a laid out version of either a component or a system with a location and size on the FPGA. There are two data constructors for `Instance`: `CmpInstance` and `SysInstance`. The component instance constructor requires the actual `Component` object, such that that component does not have to be looked up every time when processing the instance. The system instance contstructor just takes the name of the system it instantiaties, and of course the rest of the parameters an instance needs.

## `ExpcDesign`
The parse functions work towards parsing the `.expc` and `.expi` files to a `Design`, however as they need to be parsed in two separate steps this intermediate data type exists. It contains only the information that is present in an `.expc` file: a list of `haskell` blocks and a list of components.

## `Design`
This data type contains the design as it is written down by the designer. It contains the same information as the `ExpcDesign`, but also a `SystemTree`, which stores the design described in the `.expi` file.

## `SystemTree`
Represents the `.expi` file. It is a tree because one of its fields is of type `SystemTree`, hence it is a recursive datatype. This tree structure represents the hierarchy of systems that is defined in the `.expi`. `SystemTree` contains a lot of fields as there are many constructs available in an `.expi` file that later need to be elaborated.

## `System`
The elaborated version of the `Design`. Elaboration is indeed simply a function `Design -> System`. `System` is also a recursive datatype, but its recursion is a bit more hidden. A `System` has:
- A name
- A type, which is equal to the name, except for system instantiations and systems re-instantiated through chains and repeats.
- "Topdata", which is only available in the top system, and contains the information from the `.expc` file.
- Size and coordinates.
- I/O statements.
- Connections
- Constant drivers
- Elements, separated in two lists, `elems` and `allElems`, where `elems` is exactly the elements that have a location. The recursion occurs here, as an Element can be another System.

## `Element`
An element is an abstraction over components and subsystems, and contains the information where those types overlap, so an element has a name, type, size, coordinates, and I/O defenitions. Furthermore, an element has an implementation, which is either the system or the component. Using the implementation it is still possible to differentiate between component and system wherever they need a different treatment.

## `Connection`(`'`) and `CID`
A `CID` is a combination of an element name and a port name. Two `CID`s can form a `Connection`. A connection is always ordered as `Connection from to`, so even if the arrow points to the other direction, the parser constructs the `Connection` such that this holds. `Connection'` is a connection including the bitwidth of the two ports that are connected. This is useful information to have during postprocessing, and is determined during elaboration.


## `RawRepetition`
A repetition as written down by the designer. This can be either a chain or a repeat. It has a name and coordinates and a _list of options_, like the amount of repetitions and layout procedure. Since the parser does not need to enforce whether all the options are present, some options may be missing in this list. That is why during elaboration this `RawRepetition` is converted to a `Repetition`, and throws an error when options are missing.

## `Repetition`
An exact representation of a repetition like chain and repeat: in this type exactly every option is guaranteed to be present. During the construction of a value for `Repetition` errors could be thrown if any its values have not been specified by the designer.

## `MultiConnection` and `MCID`
Similar to `Connection` and `CID`, but for multiconnections. An `MCID` also has an element name and port name, but additionally has a range, which is either `All` or a range from some integer to some other integer.

A multiconnection is, just like `Connection`, two `MCID` ordered as the originating `MCID` first, and the destination `MCID` second.


## `ConstantDriver`

Similar to a `Connection`, but instead of an originating `CID`, the constant it drives is stored as a `String`. This string can later be converted to a series of bits in `Postprocessing.hs` to set the constant value to the correct ports. 

# Program Structure

This section walks through the directories of the source code in approximately the order that the program operates. For each file a discription of what kind of functions are located there and what everything is supposed to do is present.

## Compiler (`compiler/`)

Contains all the compilation flow definitions and a lot of helper functions to construct these flows.

### `Compiler.hs`

Contains the actual compilation flows of type `Flow` that are described in [setting up](setting-up.md). If you want to add a new flow, build a function of type `Flow` here. Its helper functions should be in `Flows.hs` and `Steps.hs`.

### `Flows.hs`

Contains functions that are pretty much only a monadic concatenation of `IO ()` actions. Organised as such, the flows read very much like a script. Flows defined here any arguments, using those they concatenate steps from `Steps.hs`.

### `Steps.hs`

Contains many `IO ()` actions that can be concatenated in a flow in `Flows.hs`. Each of these steps is accompanied by a neat `putStrLn` that prints what step is being taken. This way any flow prints the steps it is currently taking, informing the user of what is hapenning. This is really nice to have as some steps can take quite a long time, and it is nice to see that _something_ is happening. Some steps (like `compileToVerilog`) print intermediate messages as well.

## Parser (`parser/`)

Just as the first step of any compiler, first the source files must be parsed. In the `parser` directory all the Parsec functions necessary to parse both the component file and the instantiation file are located.

The type definitions used basically everywhere in the program are defined in a source file here as well. This could be placed in a more logical position at some point (issue [#18](https://github.com/PietPtr/Ex-PART/issues/18))

### `Parse_shared.hs`
Parser definitions shared between the `.expi` and `.expc` parser. The very hacky Haskell parsing mentioned in issue [#1](https://github.com/PietPtr/Ex-PART/issues/1) is located here, mostly in `haskell_type` and `haskell_stat`.

This also defines some standard often used parsers like `whiteSpace` and `parens`.

### `Parse_expc.hs`
`expcdesign` can parse one entire `.expc` file. As the order of statements mostly does not matter in Ex-PART, the data structures just keep lists of _types_ of statements. That's why in `expcdesign` there is a `sorter`, which groups statements according to types.

Furthermore, `isoStatement` parses **i**nput, **s**tate, and **o**utput statements. When no initial state is given, quite an unclear error message pops up. Usually if a parse error occurs in this function, it's because of a missing initial state.

### `Parse_expi.hs`
`system` parses the `.expi` file. It uses a similar strategy as in `Parse_expc` of parsing to a list of `Statement`s, and then sorting those such that it fits nicely in the `SystemTree` datatype.

The parsers here are pretty elaborate, this is because they must be able to parse all the features that are later elaborated (like chains and repeats).

### `Parser.hs`
Contains three parsers, that operate on filepaths. These make it convenient to parse the two files. `parse_both` combines the two parsers to parse two files to one `Design`.


### `Types.hs`
Types defined here are explained in the [Types](#types) section.

Besides just important types used in the project, the function mapping Haskell types to bitwidths is also located here. Regarding this function, see issue [#2](https://github.com/PietPtr/Ex-PART/issues/2).



## Elaboration (`elaboration/`)

Parsing parses to a _`Design`_, while Ex-PART operates on a _`System`_. Elaboration makes the conversion. It unrolls chains, repeats, and multiconnections, and elaborates system instantiations.

To better understand how elaboration works, compare your `.expi` file with `elaborated.expi` in the output directory. `elaborated.expi` is a pretty printed version of the `System` that your `Design` was elaborated to. In this file the comments after a connection is the bitwidth of that connection.

### `Elaboration.hs`
`elaborate` applies all the elaboration steps on a `Design` to obtain a `System`. This is more of a wrapper, as the interesting recursive elaboration function is `elaborateSystem`. That function creates a `System` record and fills in all the properties. As some of these properties contain more systems (some of which are again copies of other subsystems), this function builds up the system recursively by going through the `SystemTree`. The properties are obtained from the given `SystemTree` and `Design` on which it operates, and on the results obtained by functions in the other files in this directory.

### `Repetition.hs`
Supplies functions to unroll both kinds of repetition. It converts a `RawReptition` to a `Reptition`, which fits the list of options that the user supplied to a record containing exactly the necessary information. Once fitted, the `Repetition` is unrolled: for the elements, it generates $amount new elements (i.e. components or systems) and appends those to the system. For the chain, connections are also generated and appended to the connection list.

### `ElaborateConnection.hs`
To make Yosys postprocessing much easier, we already calculate the bitwidth of every port here. `Connection` does not have bitwidth, `Connection'` does. `elaborateConnection` finds the bitwidth for every connection. It's good to do this this early in the process as well, since many common errors regarding connections are caught by this step and that saves waiting on Clash and Yosys before discovering these.

### `Multiconnection.hs`
In a similar fashion to repeats and chains, multiconnections need to be unrolled. A multiconnection connecting _n_ ports is simply converted to _n_ connections connecting one port of the element with the correct name.


## JSON Builder (`json-builder/`)
After elaboration the system is ready to be built. The first step in the Ex-PART build process is to generate a `locations.json`, which is later used to constrain LUTs to the correct area on the FPGA.

### `Locations.hs`
Four important functions are defined here. `allInstsWithCoords` restructures the data to a new type that the rest of the file can work with. This type is a three tuple, which does not help in clarity of the code. A nicer type could be designed for this.

`hasCycle` checks whether there is a cyclic dependency in coordinate or size definitions. See also issue [#13](https://github.com/PietPtr/Ex-PART/issues/13).

`reduceAll` reduces expressions, i.e. given an expression with variables, and a mapping of variables to values, it reduces that expression to (hopefully) a constant. It uses a recursive approach for this evaluation, reducing any other expression it encounters.

`relToAbs` takes reduced expressions, which are still relative to their parent systems, and turns them into absolute coordinates on the FPGA by adding the parent's coordinates.

### `JSONBuilder.hs`
Converts the data in a `System` to something that `Locations.hs` can work with, then by combining the functions in `Locations.hs` obtains a tree of absolute positions. This is exactly what nextpnr allows for constraining, so that can be written to `locations.json`.


## Clash Generator (`clash-generator/`)
To be able to synthesize the components, they are converted to Clash. In this directory there is also code for creating simulation files: Clash files representing the entire design to verify functional correctness.

### `Generator.hs`
Wrapping functions using the `doPreliminaryProcessing` function from `Preliminary.hs` and `toClash` from `ComponentConversion` to generate Clash code for the components, and using The Flattener module to define a default `flatten` function.


### `Preliminary.hs`
Creates the `builds/` directory in which all the components will have their own directory, in which it creates directories for each component with the name of the component as the name of the directory. It also creates `Definitions.hs`, this is the file containing all `haskell` blocks of the component file. To `Definitions.hs` some preamble is added in the `genDefs` function. If you need any GHC extensions or imports enabled during simulation, this is where you could add them.

### `ComponentConversion.hs`
Converts components to Clash. For each component a function is generated, this function has the type that Clash's `mealy` function expects: `s -> i -> (s, o)`. The `toClash` function combines every step. In summary:

1. The type signature is generated from the information in the  I/S/O statements.
2. The equation is generated: the function name, state argument, input argument, and the two-tuple that a mealy machine emits in Clash is generated.
3. The where statement for this component is generated by simply copying the transition expressions, and any other expressions the designer may have specified to the where clause.
4. A `topEntity` is defined, and decorated with synthesis annotations to ensure it synthesizes in a predictable way.

To gain more insight into what exactly this file generates, take a look at an output file looking like `builds/component/Synth_component.hs` in the output directory of your project. The Clash code produced is usually quite readable.

### `Flattener.hs`
Generates simulation files by 'flatenning' the design: Starting at the top level system, it creates a Clash function where the connections and instantiations are defined exactly as in the `.expi`. Then for any subsystem it creates additional functions, recursively.

To use mealy machines defined in the `.expc` on the `Signal` level, it generates for every mealy machine with name e.g. `component` a function `componentM` that operates on the `Signal` level. 

Constant drivers are generated by creating an extra statement in the where clause like `const_0 = pure 0`, for a constant driver of 0.

A topEntity is also defined, so that the system can be converted to Verilog as one system, instead of just the components. The monolithic and hierarchic flow use this topEntity.


## Yosys (`yosys/`)
Takes care of much of the Yosys-related stuff: running the tool and applying necessary pre- and post-processing steps. The organisation of this module is a little weird: some Clash related stuff, like compiling generated Clash to Verilog also happens here (issue [#22](https://github.com/PietPtr/Ex-PART/issues/22))

### `Preprocessing.hs`
To make synthesis much faster, its better to concetanate all the loose Verilog files generated by Clash, and synthesizing that. However, if the top module does not use some module, then Yosys will not synthesize that (as an obvious optimization). To circumvent this a dummy top module is generated, that simply instantiates every component once, and routes inputs and outputs directly to and from each instance from the arguments of the top. This top module can later be deleted, and our own hierarchy of modules can be inserted. This happens in `Postprocessing.hs`.

Here definitions for Clash processes are generated as well for each component. The function `proc` from `System.Process` can simply be monadic mapped (`mapM`) over a list of these definitions to run all the processes.

### `Yosys.hs`
Contains functions to compile the Clash file for all components to Verilog, and calls the pre- and postprocessing steps. Also contains synthesis functions for the hierarchic and monolithic functions, and of course an `IO ()` action to actually run Yosys to synthesize generated Verilog

### `Postprocessing.hs`
Generates a JSON (`interconnect.json`) representing the connections and instantiations in the instantiation file. Data structures representing Yosys' JSON structure are defined first, and [Aeson](https://hackage.haskell.org/package/aeson) `ToJSON` instances are defined for these. Then, given a `System` a list of `Module`s is produced. This process is very non-trivial and very sensitive to bugs. The main problem is that Yosys defines their connections via unique integers inside a module, and Ex-PART's are defined through module and port names. Therefore a map from module and port names to these integers must be created first, and then everything needs to be arranged exactly as Yosys does in the JSON file.

This JSON goes (almost) directly to nextpnr to be placed and routed. Bugs in this part of the project, like incorrectly connecting bits in a bus, cannot be simulated anymore and are very hard to find. Manual verification of the process has occured for the Collatz example, but not for any larger designs.

### Other Files
This directory contains several `.ys` files, these are scripts for Yosys to run. `grouped.ys` is for the default flow, `monolithic.ys` and `hierarchic.ys` should be self-explanatory.

`merge_json.py` merges the Yosys JSON with just component modules and the JSON generated by Postprocessing. This is done by a Python script instead of with Aeson because it was a little bit more convenient at the time... It should of course just be done with Aeson, as that is much neater (issue [#24](https://github.com/PietPtr/Ex-PART/issues/24)).

## Nextpnr (`nextpnr/`)
Runs nextpnr on the combined JSON files, `synthesized.json`.

### `Nextpnr.hs`
Only contains one function, `nextpnr`, that runs `nextpnr` for the ECP5 with 85k LUTs. If any of the settings for nextpnr turn out to not fit your use-case, modify them here. Note that functions calling nextpnr (e.g. in `Steps.hs` or `Flows.hs`) may provide extra options for the process. 

Just as every other tool, nextpnrs output and error streams are logged to a file.

### `constrainer.py`
Nextpnr allows scripts to be run just before certain steps in placement and routing. In `Nextpnr.hs` this python script is set to be run just before placement. Any Python script nextpnr runs has access to the `ctx` object that contains all the cells to be placed, and nets to be routed. This script goes through all the cells, and based on the name of the cell, looks up where in the JSON the rectangle constraining the cell should be, finds that rectangle, and constrains the cell to that rectangle. Not every cell will have a standard name, as for example I/O cells are not part of the Ex-PART specification. These cells are therefore not constrained.

## Visualizer (`visualizer/`)

While technically not part of the program Ex-PART, the visualizer is quite an important part of the workflow. It is implemented in Python, using Pygame as a graphics library.

### `color.py`
A small library implementing some commonly used color features. As slices and much oter stuff is colored based on its name, and colors are randomizable, this is all handled centrally here.

### `init.py`
Contains everything that many modules might need. Some global zooming and viewing variables, argument parsing, pygame initialisation, and a function handling (keyboard, mouse) events.

### `files.py`
Function for monitoring and reloading `bitstream.json` and `locations.json`, the two files that the visualizer can show. There is also the drawing function for drawing indicators in the bottom right of the screen showing which file is loaded.

### `iodb.json`, `blinky.lpf` and `parse_iodb.py`
To render the IO on the sides of the FPGA, Trellis' IO database (`iodb.json`) had to be parsed and linked to names for the I/O pins. This `blinky.lpf` contains the default names of many sites of the I/O pins, and `parse_iodb.py` is a script that can parse the I/O DB, link it to names in the `lpf`, and generate an easy to visualize CSV. as long as `iodata.csv` is available and valid you probably don't need to touch these.

### `iodata.csv` and `tiledata.csv`
Some features of the ECP5 are described in these CSVs. `iodata.csv` links locations to pin names, so its easy to know exactly where a pin is on the FPGA. This helps in placing the design as it enables the designer to place the I/O of the design near the actual I/O pins. 

`tiledata.csv` contains the tiles that are _not_ LUT tiles. These tiles are also visualized in the tile grid as differently colored squares. The data for this file comes from [here](http://yosyshq.net/prjtrellis-db/ECP5/LFE5UM-85F/index.html).

### `grid.py`
Functions for drawing the grid, special tiles, and I/O pins.

### `slice.py`
Functions for drawing slices. Slices are drawn with a random color if their name abides the Ex-PART naming conventions, otherwise they are drawn in a random shade of grey.

### `routing.py`
Functions for showing usage of routing resources, by drawing darker shades of gray on tiles if more resources are used. It draws this based on a "routemap" which is regenerated if the bitstream has been updated.

### `legend.py`
Functions for drawing a legend which shows which color is used for which module.

### `connections.py`
Off by default, as computing this costs a long time for even slightly larger designs. Can be enabled with `-c`. Functions for drawing _outgoing_ connections of a component. This is a neat way to visualize _where_ the outputs of a component are located, and where the data comes in. Certainly for large components this can give insights into why placement and routing may be difficult.

### `systems.py`
Functions for drawing boxes as defined in the `locations.json`. Goes through the JSON file recursively and draws boxes for every bottom level component it finds at the specified location.

### `main.py`
Contains the main loop which calls all the functions defined in the other files for drawing, event handling, file loading, and view updating.

# Feature Implementations

Below the same feature list as in the [programming manual](programming.md) is shown. Here a brief explanation of the implementation of the feature is provided, including where to find the code.

## Comments
Technically partially implemented in GHC, as comments near transition expressions are simply copied to Haskell. For `.expi` files, Parsec does the heavy lifting. By defining a `lexer` in `Parse_shared.hs` with `haskellDef` as language definition, haskell style comments are automatically supported as whitespace. After parsing the comments have been ignored, as is the intention.

## `haskell` block
`haskell` blocks are stored in the [`Design`](#design) as a list of `HaskellDef`s. This list extracted by `genDefs` in [`Preliminary.hs`](#preliminaryhs) to generate `Definitions.hs`. This file is then imported by every other Clash file generated by Ex-PART such that the definitions can be used.

## Component definition
In `ComponentConversion.hs` most of the processing on components happens. Here they are all converted to Clash files as described [`ComponentConversion.hs`](#componentconversionhs).

## Coordinates and Sizes
All coordinates and sizes are stored as [`LayoutExpr`](#layoutexpr)s. In [`Location.hs`](#locationshs) the coordinates and sizes are processed into the `locations.json` file.

## System Definitions
System definitions are parsed in [`Parse_expi.hs`](#parse_expihs) and are then elaborated in [`Elaboration.hs`](#elaborationhs). The size and position of a system are enforced in [`Nextpnr.hs`](#nextpnrhs) and [`constrainer.py`](#constrainerpy). Connections and instances are made in [`Postprocessing.hs`](#postprocessinghs) for the ECP5, and in [`Flattener.hs`](#flattenerhs) for simulation.

## Component Instantiation
Instantiations before placement are done by instantiating cells of the component name in the Yosys JSON in [`Postprocessing.hs`](#postprocessinghs). The instantiation location and size are enforced in [`Nextpnr.hs`](#nextpnrhs) and [`constrainer.py`](#constrainerpy).

## Port connection
Port connections are made in [`Postprocessing.hs`](#postprocessinghs) by generating a `NetMap = Map CID Net`, where a `Net` is simply an integer: specifically, the unique integers Yosys uses to denote which ports are connected. For more information on Yosys' connection system, view the [Yosys manual (PDF)](https://github.com/YosysHQ/yosys-manual-build/releases/download/manual/manual.pdf), in appendix C.218. The `NetMap` thus assigns an integer to ports, the same integer is assigned to two ports if they are connected via a `Connection`.

## Constant Drivers
Constant drivers are implemented in [`Postprocessing.hs`](#postprocessinghs), there a Yosys JSON module is generated for a constant driver, which is instantiated in the correct place to implement the design. For simulation lines like `const_0 = pure 0` are added to where statements of systems containing constant drivers in `Clash.hs`.

## Repeat and Chain statement
Repeat and chain statements are fully implemented during [elaboration](#elaborationhs) as they are simply unrolled to simpler elements.

## Multiconnections
Multiconnections are unrolled to simple single connections during [elaboration](#elaborationhs). From then on the generated connections are indistinguishable from regular connections.

## Unplaced Systems
A `System` has two fields for elements, one which contains _all_ elements, including the unplaced ones, and one which contains just the elements which need to be put somewhere. When instantiating a system the field with all elements is filtered on the name of the system that need to be placed, and when [`Postprocessing.hs`](#postprocessinghs) actually instantiates all the systems only the field with the placed elements are instantiated.

## System Instantiation
During [elaboration](#elaborationhs), if a system instantiation is encountered, the system is simply copied, but with a different name. 


# Error List

This lists errors that may be thrown that are not listed in the [programming manual](programming.md). If you see any of these errors, there is most likely a bug in Ex-PART, and not in your code. The information here may point you to where to search for a solution.

## Errors

- `clash-generator/ComponentConversion.hs:82:` A state is not a port.
  - During component conversion, an `ISOStatement` constructed as an SState was given to the function `toPortName`. This function is a local function, and is called only on filtered lists of `ISOStatement`s such that either only inputs or only outputs are passed, hence this error should never appear.
- `clash-generator/Preliminary.hs:38:` Something went wrong during elaboration, the top system does not have top-data.
  - Elaboration generates a `System`, which has a property `topdata`. The idea is that the root node of the `System` hierarchy has the `topdata` set to the information in the expc file. If this hasn't hapenned, something is broken in `Elaboration.hs`.
- `compiler/Compiler.hs:100:` How can there be several components with the same name?
  - This code is part of the `auto` flow, which is a little bit broken. No research has been done where exactly it breaks as it is quite time-consuming and hard to reproduce (issue [#7](https://github.com/PietPtr/Ex-PART/issues/7)).
- `json-builder/Locations.hs:101:` Coordinate reduction found non-constant value ($expr)
  - The reduction should only be run on constant coordinate expressions, so any width/height/x/y variable must have been substituted by a constant value. Probably the error "Could not find ID in provided list" will be thrown earlier than this one, as that appears in a function that turns expressions into expressions without variables, and that function is always called before the function this error appears in.
- `nextpnr/Nextpnr.hs:28:` nextpnr terminated with code $code
  - This error may occur when assertions in nextpnr are tripped, and these can be tripped by incorrectly constraining LUTs, e.g. to a "negative" area rectangle (by setting reversing the top left and bottom right). It may also be the case that something weird was defined in the JSON generated by [`Postprocessing.hs`](#postprocessinghs).
- `parser/Types.hs:102:` Invalid ISOStatement for bitwidth (state not implemented)
  - In the current implementation of Ex-PART, the bitwidth of states is irrelevant. Therefore no conversion to bitwidth from states is implemented. This function _probably_ is never called with a state, but the types cannot guarantee this right now.
- `yosys/Preprocessing.hs:57:` No state should have been seen here.
  - Again the issue with ISOStatements appearing where only I/O is relevant... (issue [#21](https://github.com/PietPtr/Ex-PART/issues/21)) This function is again only called with filtered statement lists, so this error should never trip. These next two errors are to catch the same problem, just more specifically for either only inputs or only outputs.
  - `yosys/Preprocessing.hs:68:` Not an input: $x
  - `yosys/Preprocessing.hs:74:` Not an output: $x