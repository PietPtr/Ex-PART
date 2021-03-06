- [Designing Hardware with Ex-PART](#designing-hardware-with-ex-part)
- [General Remarks](#general-remarks)
- [Glossary](#glossary)
- [Design Feedback](#design-feedback)
  - [Visualisation](#visualisation)
    - [Example commands](#example-commands)
    - [Controls](#controls)
  - [Metric Analysis](#metric-analysis)
  - [The Output Directory](#the-output-directory)
  - [`Clash.hs`](#clashhs)
- [Thorough Explanation of a basic Ex-PART program](#thorough-explanation-of-a-basic-ex-part-program)
  - [`.expc` file](#expc-file)
  - [`.expi` file](#expi-file)
- [Simulation with Clash](#simulation-with-clash)
- [Post-Synthesis Simulation](#post-synthesis-simulation)
- [Bitstream Generation](#bitstream-generation)
- [Feature List](#feature-list)
  - [Comments](#comments)
  - [Types](#types)
  - [`.expc` file](#expc-file-1)
  - [`haskell` block](#haskell-block)
  - [Component definition](#component-definition)
  - [`.expi` file](#expi-file-1)
  - [Coordinates and Sizes](#coordinates-and-sizes)
  - [System Definitions](#system-definitions)
  - [Component Instantiation](#component-instantiation)
  - [Port connection](#port-connection)
  - [Constant Drivers](#constant-drivers)
  - [Repeat statement](#repeat-statement)
  - [Chain statement](#chain-statement)
  - [Multiconnections](#multiconnections)
  - [Unplaced Systems](#unplaced-systems)
  - [System Instantiation](#system-instantiation)
- [Error Messages](#error-messages)
  - [Errors List](#errors-list)

# Designing Hardware with Ex-PART

In this guide as much as possible information on how to _design hardware_ in Ex-PART is provided. Since Ex-PART was developed for a Master thesis, there are quite some hacks and pitfalls you may encounter and there are a wide variety of features available.

This guide fully focusses on allowing you to design hardware in Ex-PART. If anything goes wrong (crashes, unexpected results, etc), take a look at the maintenance manual to find out how to continue, and where you may need to apply a fix.


# General Remarks

- Haskell syntax highlighting works quite well with Ex-PART, it's not perfect but as there was not really a consistent grammar available no custom syntax highlighting was developed.

- Parse errors are usually not very clear, see issue [#10](https://github.com/PietPtr/Ex-PART/issues/10).

- Parse errors may occur when spaces or other whitespace occurs in the wrong place. The parser does not allow some kinds of statements to end with whitespace, for example.

- Parse errors will also occur when the character '}' occurs unexpectedly, since this character is used to detect the end of a block (like a `haskell` block, as will be explained later). This character occurs in Haskell multiline comments and in records, hence these cannot be used in Ex-PART.
  
- If parse errors in Haskell statements happen because of other reasons, it's possible that you used a character that is not mentioned in `haskell_stat` in `Parse_shared.hs` (See also issue [#1](https://github.com/PietPtr/Ex-PART/issues/1))

- Bitwidths of types are not determined automatically. In `parser/Types.hs` a switch statement is located that matches the name of a type to a bitwidth. Usually this becomes clear by the error "cannot find bitwdith for type `<Type>`". This means that if you use any type that it is not listed there, or define a type synonym of the same name but with a different bitwidth that is defined there, Ex-PART will not function correctly (Issue [#2](https://github.com/PietPtr/Ex-PART/issues/2)). If you want to add a type, either solve this problem correctly by looking at how Clash determines type bitwidths, or simply add / modify the case statement.

- Scopes are very unclear. No time was spend on designing some exact scoping of variables, and in practice there are several namespaces in Ex-PART (even though they were not necessarily built on purpose). There is the namespace of component/system instantiations, these are given a name such that they can be referred to in coordinate expressions. These names are globally referencable in coordinate expressions, but no error will be given if they do overlap, simply the 'first' will be returned. Then there is a namespace of system and component type names, which is used to designate the kind of system instantiated when instantiating something. This is also globally referenceable. If anything goes wrong in scopes, it's likely  that there is a bug, try renaming variables such that everything has a unique name and see if that goes better. Furhermore, I have not checked whether scopes in the simulation and in synthesis behave the exactly the same. See also [#11](https://github.com/PietPtr/Ex-PART/issues/11).

- The feature list below also details some hacks, tips, and tricks for these features that may ease development or help understanding why something behaves unexpectedly.

- When driving the same input with two outputs, none of the tools will give warning, the design is just synthesized to ... something. Beware.

- Overlap between components is allowed in Ex-PART. Experience shows that it is often very useful to have two components share area; if they are thoroughly interconnected it is much easier to find a good placement if the LUTs can be in the same rectangle. 

# Glossary

- **Component**: a mealy machine defined in the component file (`.expc`).
- **Element**: Anything with input and output ports that can be laid out on the FPGA: so either a component or a system.
- **Instance**: a particular occurence of some element, with a position and size, on the FPGA.
- **System**: a hierarchy of systems defined in the instantiation file (`.expi`), corresponds loosely to a _module_ in Verilog, or a function in Clash operating on `Signal`s.

# Design Feedback

To aid in designing with Ex-PART, some helper tools are available. These give much information on your design _while_ you are working on it.

## Visualisation

The Python program `visualisers/main.py` is a visualizer for two types of output JSON files of Ex-PART. It can visualize component placement (taken from the generated `locations.json`) and placement of slices onto the FPGA taken from an output JSON generated by nextpnr (consistently named `bitstream.json` throughout all Ex-PART builds). `visualize.py` expects one argument: the folder in which it should search for JSONs. When none are found or the folder does not exist, the program simply shows an empty screen. As soon as either one of the JSONs is available it will be drawn by the visualizer. To differentiate between location information as given in the `.expi` and placement information as generated by Ex-PART, `.expi` colors are more muted and drawn as a border. If the information is available, a legend will be drawn in the topright corner. Indicators in the bottom right corner show which files are loaded.

### Example commands

`python visualizers/main.py project`

Shows bitstreams and locations in project folder `project/`, as soon as they are generated.

`python visualizers/main.py project/builds/component/`

Shows out-of-context (i.e. I/O is not routed to) pnr result of component `component` in project `project`, as soon as it is generated.

`python visualizers/main.py project/monolithic/`

Shows bitstream as generated by the monolithic flow for project `project`. 

The visualizer can always be run and does not need the folder to exist, it simply waits until the folder contains visualizable files. File loaded indicators in the bottom right corner indicate which visualisable files were found.

### Controls
Click-and-drag works to pan around the design. Scrolling zooms in and out. Pressing `F1` re-randomizes all the colors (can be useful if some colors look too much alike or are just plain ugly). `F2` takes a screenshot and saves it as "\<unix-timestamp\>.png". Pressing `R` sets the coordinate ranges to relative mode: the tile your mouse hovers over is then (0, 0). This feature is currently broken if the view was panned ([#23](https://github.com/PietPtr/Ex-PART/issues/23))

Notes:

- The visualiser will try to visualize any folder that contains either a `locations.json` or a `bitstream.json`, so it also works on the output directories of the monolithic, hierarchic, and resource flow. 

- There is a switch, -c, to enable showing connections that leave a component. This will render every connection that goes from one cell in a component to a cell in some _other_ component. 

- There is also a switch, -p, to enable a simpler view that shows the full ECP5 at once.

- The visualiser is built entirely for the 85k version of the ECP5. More information on that version of the ECP5 can be found [here](http://yosyshq.net/prjtrellis-db/). For more comments see issue [#9](https://github.com/PietPtr/Ex-PART/issues/9).

## Metric Analysis

All the tools Ex-PART runs to obtain results emit outputs and errors that may be valuable for debugging and evaluating a design. Since these are spread all over the place, a script gathering these metrics is available in the root directory: `analyze.py`. Simply run it with the name of an Ex-PART output directory as its argument and it will search all available logs for metrics like LUT/FF usage, maximum frequency, and runtimes. If it can't find a metric it will print it as a dash. If hierarchic and monolithic output folders are available it will print the metrics in a table to easily compare them. Just as the visualiser, this script can be run on directories in the `builds/` directory to gain insights into resources used by components.

## The Output Directory

A ton of information is available in the output directory, all the logs, intermediate files, simulation files, etc. If anything goes wrong, do look at those files as some errors may have ended up only there and not in the output of Ex-PART.

## `Clash.hs`

For every project a `Clash.hs` file is generated, this is intended for simulation, but running it with Clash can also catch type- and other errors.

# Thorough Explanation of a basic Ex-PART program

In this chapter we will walk through the collatz example (`examples/collatz/`). This file describes a piece of hardware that keeps a number in a 16 bit register, and applies the rules of the [Collatz conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture), i.e. if the number in the register is even it is divided by two, and if it is odd it is multiplied by three and one is added to it. 

In the image below the architecture is shown. Every box is a mealy machine as will be defined in the component (`.expc`) file, and every line is a connection between I/O ports of the components. Lines are annotated with the _type_ of the output and input port they connect.

![Collatz conjecture calculator architecture](./resources/collatz.svg)

## `.expc` file

Now, an explanation of the [`.expc` file](../examples/collatz/collatz.expc), starting at the top:

```haskell
haskell {
(>>>) :: Bits a => a -> Int -> a
(>>>) = shiftR

(<<<) :: Bits a => a -> Int -> a
(<<<) = shiftL

type Value = Unsigned 16

}
```
In a `haskell` block arbitrary Haskell code can be added to the design. Each component has access to these definitions. Do not indent them as they are copied straight to a Haskell file. Define helper functions, type and data definitions, and debugging functions here. By default, `Data.List` and `Clash.Prelude` are available, and the extension `NumericUnderscores` is enabled.

```haskell
component router() {
    input val : Value
    output odd : Maybe Value
    output even : Maybe Value

    even = if testBit val 0 then Nothing else Just val
    odd  = if testBit val 0 then Just val else Nothing
}
```
A component definition. After the keyword `component` the name of the component is defined, followed by `()`. The parentheses are necessary as a feature was planned to allow generics to be passed to a component. This feature has not been implemented, but some support for it remains there in the parser (issue [#19](https://github.com/PietPtr/Ex-PART/issues/19)). 

The first lines of a component are the input, output, and state definitions. This component happens to be a combinational component: it has no state. This component receives a 16-bit value (namely `val`, of type `Value`, which was defined to be an `Unsigned 16` in the `haskell` block earlier). It has two outputs, both of type `Maybe Value`.


```haskell
component onEven() { ... }
component onOdd() { ... }
component merger() { ... }
```

These components are also all combinational, so not much news happens here, so their implementation is ommitted.

```haskell
component control() {
    input next_val : Value
    input set_val : Maybe Value
    state last_val = 0 : Value
    output result_value : Value

    last_val' = case set_val of
        Just new_value -> new_value
        Nothing -> next_val

    result_value = last_val
}
```
This is a component with state. Its state is defined in the fourth line. It is given a type just like the inputs and outputs (`Value`). Additionally an initial value is supplied, namely `0`.

To define the state transition, an expression is defined for `last_val'`. Notice that this expression can depend on any of the inputs, and the previous state. It can also depend on some other state, or their next states, as long as they do not form a mutually recursive dependence. 

## `.expi` file

With the components defined, the `.expi` file can be written to layout those component on the FPGA.

```haskell
system in (6, 5) at (2, 2) {

```
The top-level system is called `system`, takes up an area of six by five, and is located at position (2, 2) on the ECP5. This coordinate system is zero-indexed, (0, 0) is the top left corner. Ex-PART uses the same system as the [HTML documentation of the ECP5](http://yosyshq.net/prjtrellis-db/).

The size of systems is not checked by Ex-PART, if you specify (1, 1) here it may work as well. Where it is taken into account is when any of the components _refer_ to this value, that is if a component is placed at e.g. `(system.x, 0)`. This size is currently not in any way inferable, if you resize components in a hierarchy and you need to use an accurate size for the system, you need to update the system size manually. See also issue [#12](https://github.com/PietPtr/Ex-PART/issues/12) on inferable sizes.

```haskell
    input setting : Maybe Value
    output result : Value
```
The I/O of the top-level system. As the input we define `setting`, this `Maybe Value` can set the value in the register to which the Collatz conjecture rules must be applied. `control` is a component that has an input of type `Maybe Value` for exactly this purpose, so once that component is instantiated we must route this input to that component.

Since this is I/O of the _top_-level system, this is also the I/O that must be constrained in the `.lpf` file. 

```haskell
    controller is control in (6, 1) at (0, 0)
```
The `control` component is instantiated. It is given the name 'controller', an area of six by one, and the location _(0, 0)_. This location is relative to the system it is a child of, so on the FPGA this component will be located at (2, 2).

The size could also have been defined as `(system.w, 1)`, for example.

```haskell
    controller.set_val<-setting
    controller.result_value->result
```
The controller's inputs are linked to the inputs of its parent system. Notice that the arrow notation can go both ways. A port of the control component is referred to by writing the _name_ of the instance of the component, followed by a period, and then the port name. 

Local system ports do not need this period-syntax, they are simply referred to by name, as is done with `setting` and `result`.

With these statements the system I/O port setting and result are connected to the controller, so that the controller component drives/is driven by the FPGA I/O ports as intended.


```haskell
    collatzer in (controller.w, 4) at (0, controller.h) {
```
A subsystem with the name `collatzer` is defined. At this point it is probably a good idea to view the file this comes from, as the indentation here will make it much clearer that this is a _sub_-system. Note that its size and position is defined in terms of the controller, so if we resize the controller, this entire system moves with it.

```haskell
        input val_in : Value
        output val_out : Value
```
This subsystem also has its own input and output ports. Since the goal of this system is to calculate the next value of its input according to the rules of the Collatz conjecture, the types of the single input and output port are both `Value`.

```haskell
        router is router in (1, onOdd.h + onEven.h) at (0, 0)
        onOdd is onOdd in (collatzer.w - 2, 2) at (collatzer.x + 1, 0)
        onEven is onEven in (onOdd.w, onOdd.h) at (onOdd.x, onOdd.h)
        merger is merger in (1, onOdd.h + onEven.h) at (onOdd.x + onOdd.w, 0)
```
All the necessary components are instantiated. To showcase what kind of expressions are possible in the coordinate and size expressions they have been written to be highly dependent on other values. Notice that both components (like `onOdd`), a system (`collatzer`), and constants are used in these expressions. See also issue [#13](https://github.com/PietPtr/Ex-PART/issues/13) and [#14](https://github.com/PietPtr/Ex-PART/issues/14) for more information on what you cannot do with these expressions.


```haskell
        router.val<-val_in
        router.odd->onOdd.val
        router.even->onEven.val
        onOdd.res->merger.vo
        onEven.res->merger.ve
        merger.res->val_out
    }
```
To finish up the subsystem `collatzer` the connections are defined as in the diagram shown at the start of this chapter. If you try to connect ports of different types, Ex-PART will throw an error.

```haskell
    collatzer.val_in<-controller.result_value
    collatzer.val_out->controller.next_val
}
```
The I/O ports on the subsystem need to be connected to the controller, and that is exactly what we do here. The ports of the subsystem can be referenced by using the name of the subsystem, a period, and then the port name.

# Simulation with Clash

The following flows generate simulation files: `clean`, `auto`, `monolithic`, `hierarchic`, and of course `sim` (which is there _just_ for simulation file generation).

"Simulation files" in Ex-PART are simply Clash files, i.e. Haskell code. After using either one of these flows you will find the files `Definitions.hs` and `Clash.hs`. `Definitions.hs` contains everything you put inside `haskell` blocks, and `Clash.hs` contains all the mealy machines representing components, and functions representing the hierarchies of systems in the `.expi` file. If you're familiar with Clash it may be very useful to inspect these generated files when checking for functional correctness. They have been generated in a way that they ought to be relatively readable. Furhermore, in the output directory a directory called `builds` is created, which contains a directory for every component in the design. These directories also contain Clash files for just those components. These are called `Synth_<component>.hs`, where `<component>` is the component name.

To simulate your entire project, use the command:

```shell
clashi Definitions.hs Clash.hs
```

To simulate just one component with the name `component`:

```shell
clashi Definitions.hs builds/component/Synth_component.hs
```

Once in the `clashi` shell, test for functional correctness as usual using Clash's `simulate`, `sample`, etc. functions.

# Post-Synthesis Simulation

Since Ex-PART operates directly on the JSON file that Yosys outputs, no post-synthesis simulation is available.

# Bitstream Generation

Ex-PART uses nextpnr to generate bitstreams. Nextpnr emits bitstreams in both JSON format and the Trellis textual configuration format. Both of these versions are available in the output directory as `bitstream.json` and `bitstream.config`. With the program `ecppack` (which is part of [project Trellis](https://github.com/YosysHQ/prjtrellis)) `bitstream.config` can be converted from a textual representation to a bitstream that can be programmed to an ECP5 FPGA.

Nextpnr warns that it is experimental software and that it _might_ break your FPGA. Ex-PART is (clearly) even more experimental, and uses nextpnr. Before flashing any designs Ex-PART generated to your FPGA, you should really try to manually verify nothing weird is going on in both the `bitstream.json` and the `synthesized.json`.

# Feature List

In this chapter you find a comprehensive feature list of the language Ex-PART. For every feature a brief explanation is provided, syntax examples are given, and the [example designs](../examples/) which use the feature are listed. For a short description of their implementation the same list is available in [the maintenance manual](maintenance.md).

## Comments

Comments are a little iffy in Ex-PART. In .expi files single line comments usually work:

```haskell
a is a in (5, 3) at (0, 0) -- this is a single line comment
```

In expc files comments at least do always work in the expression statement parts, as those are directly copied to Haskell code during compilation. However, comments outside components and between input, output and state definitions _may_ not work. Multiline comments do not work, as the `}` character is used to determine if a component definition has finished (issue [#31](https://github.com/PietPtr/Ex-PART/issues/31)).

```haskell
-- ?? This comment fails
component router() {
    input val : Value
    -- ?? This comment fails
    output odd : Maybe Value
    output even : Maybe Value

    -- ??? A working comment
    even = if testBit val 0 then Nothing else Just val -- ??? This comment is fine
    odd  = if testBit val 0 then Just val else Nothing
    {- ?? This multiline comment
         causes problems
    -}
}
```

## Types

In both component definitions and system definitions ports and states need to be annotated with a _type_. These types are exactly Clash types, so it is always possible to give ports any type Clash supports. Do note that the parser of the types in Ex-PART does not support everything, so to circumvent any parse errors you may get in types, define a type synonym in a `haskell` block. For example, this line may not parse:

```haskell
    input inp : Maybe (Vec 4 (Unsigned 16))
```

Solve this by defining this `haskell` block:

```haskell
haskell {
type SomeInputType = Maybe (Vec 4 (Unsigned 16))
}
```

And changing the erroneous line to:
```haskell
    input inp : SomeInputType
```


## `.expc` file
The component or `.expc` file is where components and generally available Haskell code is defined. Every example contains an `.expc` file as without components no hardware can be described.

## `haskell` block
In a `haskell` block general Haskell code can be written. Any component can use the function or type definitions defined in such a Haskell block.

In this example two synonyms for the Clash functions `shiftR` and `shiftL` are defined, and a type synonym for `Unsigned 16` is set.

```haskell
haskell {
(>>>) :: Bits a => a -> Int -> a
(>>>) = shiftR

(<<<) :: Bits a => a -> Int -> a
(<<<) = shiftL

type Value = Unsigned 16
}
```
This is useful because now it is possible to easily change the bitwidth of every port and state in the components simply by changing the type to which `Value` aliases.

Do not indent any code in this block, as Clash will give errors during Verilog compilation or simulation.

Every example uses a `haskell` block since basically always you'll need to define some helper functions and types / type aliases. There may be a bug as well regarding `.expc` files without a `haskell` block: issue [#4](https://github.com/PietPtr/Ex-PART/issues/4).

## Component definition

A general component definition looks as follows:

```haskell
component <name>() {
    input <input n>                     : <input_type n>
    state <state n> = <initial_state n> : <state_type n>
    output <output n>                   : <output_type n>

    <state n>' = <state_expr n>
    <output n> = <output_expr n>
}
```
Where `<text n>` means that there can be any number `n` of such statements.

- `<input n>`: the name of some input port.
- `<input_type n>`: the type of some input port.
- `<state n>`: the name of a state.
- `<initial_state n>`: the value for the initial state of that state. The parser is quite weak for this initial state (e.g. `Just 0` may not work), define a constant function in a `haskell` block to circumvent this (e.g. `my_initial_state = Just 0`, and setting the initial state to `my_initial_state`). When you forget this value, a very unclear parse error pops up. When these statements don't parse it's usually because the initial state was left out.
- `<output n>`: the name of some output port.
- `<output_type n>`: the type of some output port.
- `<state_expr n>`: the transition expression for this state. This expression is simply a Haskell expression. Any Haskell construct can be used here, except for records, as they contain the character `}` in their syntax (see also issue [#15](https://github.com/PietPtr/Ex-PART/issues/15)). A transition expression can use any of the variables that are in scope in the component: inputs, other states, even the next values for other states, as long as there is no mutually recursive dependency between them. To find out if this has happened (on accident), run the Clash simulation. If it produces no output there probably is such a mutually recursive pair in a component somewhere.
- `<output_expr n>`: the transition expression for some output.

Notice that there is a prime (`'`) after `<state n>` in the equation for the transition expression! The new state for an old state `state` is always called `state'`.

Example components are given in the ["Thorough Explanation of a Basic Ex-PART Program"](#thorough-explanation-of-a-basic-ex-part-program) section, and of course every example project contains many components.

## `.expi` file

In the instantiation or `.expi` file the components defined in the `.expc` file are laid out on the two-dimensional grid of the FPGA. 

## Coordinates and Sizes

Every system definition and component instantiation has a size and location. These are both given as two-tuples, `(width, height)` for the size and the `(x, y)` for the location. In these tuples _layout expressions_ can be used. The operators `+` and `-` are available to define layouts. Furthermore, layout properties of other components can be used. The layout properties for a component called `component` are as follows:

- `component.w`: the width of `component`
- `component.h`: the height of `component`
- `component.x`: the x coordinate of `component`
- `component.y`: the y coordinate of `component`

There exists somewhat of a global scope for these variables: 'somewhat' because Ex-PART does not give errors if any names in this scope overlap, it simply returns the first value it finds.

Parentheses can be used to impose precedence on subexpressions as usual in arithmetic expressions.

In almost every example many examples can be found of these expressions being used.

See also issue [#13](https://github.com/PietPtr/Ex-PART/issues/13) on cycle checking in coordinate and size expressions.


## System Definitions

A system definition is of the following form:

```haskell
<system_name> in (<system_width>, <system_height>) at (<system_x>, <system_y>) {
    input <input n> : <input_type n>
    output <output n> : <output_type n>

    <instantions>
    <connections>
    <subsystems>
    <elaborated_features>
}
```

- `<system_name>`: the name of this system, can be any identifier.
- `<system_width>`: the width of the system.
- `<system_height>`: the height of the system.
- `<system_x>`: the x coordinate of the system.
- `<system_y>`: the y coordinate of the system.
- `<input n>`: some input of the system.
- `<input_type n>`: the type of some input of the system.
- `<output n>`: some output of the system.
- `<output_type n>`: the type of some output of the system.
- `<instantions>`: component or system instantiations. The order of statements of this and the following three kinds does not matter.
- `<connections>`: connections between ports of systems and components.
- `<subsystems>`: subsystem definitions. A definition for a subsystem is exactly the same as a system, just indented by one more level (by convention).
- `<elaborated_features>`: Some extra syntactic sugar is available to make development of repetitive designs easier, these features are all "elaborated" to a collection of the previous three kinds (instantiations, connections, subsystems).

Top-level systems have some more requirements/caveats: the width, height, x, and y expressions _must_ be constant expressions: they cannot depend on any other component or system. Furthermore, the ports defined as I/O ports in the top-level system are the ports that must be constrained to the FPGA's I/O pins in the `.lpf` file.

## Component Instantiation

The basic way to layout components is as follows:

```haskell
<instance_name> is <component_name> in (<width>, <height>) at (<x>, <y>)
```

- `<instance_name>`: the name given to this instantiation. May be the same name as the component that is instantiated.
- `<component_name>`: the name of the component that is to be instantiated, this is the name of one of the components in the component file. 
- `<width>`, `<height>`, `<x>`, `<y>`: layout expressions defining the position and size of this instance.

## Port connection

Ports of instances and systems can be connected as follows:

```haskell
<from_element>.<from_port>-><to_element>.<to_port>
<to_element>.<to_port><-<from_element>.<from_port>
-- Example:
component.output_port->component2.input_port
```
Notice the `.` and `->`/`<-`. The period signifies that the port to its right is a port of the element to its left. The arrows denote connection. Both directions are available, use whichever is more convenient to read or write at the moment.

An "element" refers here to anything that has ports: systems, components, and subsystems.

Ports of the current system are referred to without any element:
```haskell
<system_input>-><to_element>.<to_port>
<from_element>.<from_port>-><system_output>
-- Example:
system.output->local_output
```

View the example section and code examples for many examples on connection. There is some extended syntax for so-called [multiconnections](#multiconnections) and [constant drivers](#constant-drivers), explained in their respective sections.

## Constant Drivers

When some input of a component must be driven by a constant value, you would have to define a mealy machine with one output whose transition expression is a constant. This is quite cumbersome, so shorthand is available to do this:

```haskell
router.x<-(4)
```
Taken from the manycore example, where a router must know its own position, and that value is simply constantly driven to the `x` input port of the router. A constant driver is of the form `(<constant>)`, and may only appear on the dash-side of the arrow (obviously, as otherwise you would route the output of some component to a constant value, that doesn't make any sense). 

Constant driver support is quite limited at the moment: it is only possible to drive numbers constantly. It would be preferable to have that be any constant value available in Haskell, but as Haskell allows defining your own data types that would result in a bit more complicated parsing.

If you're willing to give up simulation it _is_ possible though. Let's suppose we have a component with a port of type `(Maybe (Unsigned 4))`. Suppose we want to drive `Nothing` on it constantly. Since `Nothing :: Maybe (Unsigned 4)` is represented as '`0....`' (cf. `pack (Nothing :: Maybe (Unsigned 4)))` in Clash), by driving the constant `(0)` we will get the intended effect in hardware. This is not simulatable since the Clash code will throw a type-error, as `0` is not of type `Maybe a`.

Another approach that does preserve simulation is defining a mealy machine as follows in the `.expc` file:

```haskell
component constantNothing() {
    output c : Maybe (Unsigned 4)

    c = Nothing
}
```

Instantiate the component anywhere, and connect its `constantNothing.c` port to the port that must be driven by Nothing.

See issue [#16](https://github.com/PietPtr/Ex-PART/issues/16).

Examples using constant drivers: manycore, chain, core, constants.

## Repeat statement

The repeat statement simply _repeats_ a component or system several times. In Clash this corresponds loosely with a map. Its syntax is as follows:

```haskell
    repeat <repeat_name> at (<x>, <y>) {
        component = <element> in (<width>, <height>),
        amount = <amount>,
        layout = <layout>
    }
```

- `<repeat_name>`: The name of this repetition. This name is referred to in [multiconnections](#multiconnections).
- `<x>`, `<y>`: The location of the _first_ instance in this repetition.
- `<width>`, `<height>`: The width and height of _every_ instance in this repetition.
- `<element>`: The component or subsystem name to be repeated. A subsystem can be repeated too, if you have instantiated some system somewhere it is possible to _also_ lay it out using repeat as well. For more info see the section on [unplaced systems](#unplaced-systems).
- `<amount>`: How often the element must be repeated.
- `<layout>`: either `horizontal`, `vertical`, or `identical`. These describe how the layout must be continued: in a horizontal or vertical line, or placing every component at the same position.

[This image](resources/repeat_result.png) shows an example of a vertical layout of some component of size (4, 2), at location (2, 2), with amount five.

The order of the settings does not matter.

Once instantiated, individual components of the repetition can still be addressed:

```haskell
<repeat_name>[<index>].<port>->some_port
```

Where `<repeat_name>` is the name given to the repetition, and `<index>` is a **1-indexed** accessor for the components, **so the first component in the repeat with name `repetition` is `repetition[1]`**. This is 1-indexed since this is _not_ an array in memory, this is a line of components laid out in 2D space. When, in real life, some line of objects is laid out, it is most natural to refer to the leftmost or topmost component as the "first", not the "zeroth". Furthermore, with 1-indexed component references the last component index is equal to the amount, which is again quite natural: if there are in total five objects somewhere, you refer to the last as the "fifth" in natural language and hence as `repetition[5]` in Ex-PART.

Accessing more than one component at the same time as possible with [multiconnections](#multiconnections)

Coordinate and size expressions do not support indexing in the parser. It is possible to circumvent this: internally a statement such as `repetition[1]` is translated to an instance with the name `repetition_1`, and this identifier _is_ available in coordinate and size expressions. This hack is used in the manycore. See also issue [#17](https://github.com/PietPtr/Ex-PART/issues/17).

Examples using the repeat statement: repeat, manycore, chain, core, smallnet, router.

## Chain statement

Use the chain component to build _chains_ of components. In Clash this corresponds loosely to a fold. Its syntax is as follows:

```haskell
    chain <chain_name> at (<x>, <y>) {
        component = <element> in (<width>, <height>),
        amount = <amount>,
        layout = <layout>,
        chain_in = <chain_in_port>,
        chain_out = <chain_out_port>
    }
```

The diagram below shows what hardware this generates.

![Chain diagram](resources/chain.svg)

Given the top component, with an some input port `<chain_in_port>` and an output port `<chain_out_port>` _of the same type_, the `chain` primitive builds a chain of `<amount>` components, connecting the `<chain_out_port>` of component $n$ to the `<chain_in_port>` of component $n+1$. 

Other inputs and outputs may be available, as shown in the diagram. These still can be accessed using the same access syntax as in the repeat statement. Accessing more than one component at the same time is possible with [multiconnections](#multiconnections)

Chain is a special (more elaborate) case of repeat, so whatever holds for repeat is usually also true for chain.

This is used in the examples: manycore, chain.

## Multiconnections
To easily connect many ports of chains or repetitions of components to other chains and repetitions (to e.g. create a two-dimensional grid of hardware) multiconnections are available. They can take several forms:

```haskell
<from_repetition>:<from_port> -> <to_repetition>:<to_port>
-- Example:
drivers:out -> sum_chain:next
```
Using the `:` with a repetition instead of a `.` denotes that for _every_ component in the `<from_repetition>`, the port `<from_port>` must be connected to the `<to_port>` of every component in the `<to_repetition>`. This only works when both repetitions are of exactly the same size. When they are not the following syntax is available to select parts of a range:

```haskell
<from_repetition>[<from_index>-<to_index>]:<from_port>
-- Example:
repetition[1-3]:port
```
This denotes that the `<from_port>` in every component with index ??? `<from_index>` and ??? `to_index` are connected. More examples of this usage are available in the repeat and manycore example.

This is used in the examples: manycore, chain, repeat.

## Unplaced Systems

As mentioned, it is possible to chain or repeat a _system_ instead of just components. However, to define a system hierarchy implies immediately instantiating it as well. When you just want a chain of the same system hierarchy, and not one extra system somewhere, it is possible to add the qualifier `unplaced` before a system definition:

```haskell
unplaced <system_name> in (<width>, <height>) { ... }
-- Example taken from manycore.
unplaced pru in (28, 14) { ... }
```

This qualifier tells Ex-PART that this system hierarchy may be reused or instantiated _somewhere_, just not here. 

This is done in the manycore example.

## System Instantiation

Systems can also be re-instantiated using similar syntax to components. This allows you to define a system, and then add several more instances of the system. This is done in the md5_reuse example: first the MD5 hashing hierarchy is defined as the subsystem named `hash1`. Then under that system three statements are located that instantiate three more of the same system to build a design that can perform the same calculation four times in parallel.

```
<instance_name> is <system_name> in (<width>, <height>) at (<x>, <y>)
```

Notice that this is exactly the same syntax as component instantiation. The only difference in semantics is that now we use a _system name_ instead of a component name. This `<system_name>` is one of the systems in the `.expi` file, and may be [`unplaced`](#unplaced-systems).

# Error Messages

Ex-PART's error system is _very_ simple: it uses haskell's `error :: String -> a` function whenever anything unexpected happens (see issue [#20](https://github.com/PietPtr/Ex-PART/issues/20)). Occasionally Ex-PART will dump extra output in the error message as well. This list aims to go through all error messages Ex-PART may throw and provide a short explanation on why this error may be thrown and what can be done to fix it.

If an error is not mentioned here, it is probably in [the maintenance guide](maintenance.md), as it may be indicative of an error in Ex-PART instead of in your `.expc` or `.expi`.

## Errors List

- `No such file or directory: /usr/share/ex-part/<filename>`
  - Make sure you have run `make install` or `make symlink`, as explained in [setting up](setting-up.md).
- `clash-generator/Flattener.hs:131:` No connection specified for element $name (is $type), port $portname
  - The flattener searched for a driver for the port $portname of element $name (which is of component or system type $type), but could not find it. Check if that port is indeed connected to something.
- `clash-generator/Flattener.hs:151:` No connection specified for io statement $io in system $sysid
  - Flatenner couldn't find a driver for the IO port $io of a system called $sysid. Also prints all the connections that it did find. Check if that port is connected.
- `compiler/Compiler.hs:91:` No components in expc file...
  - An .expc file must contain at least one component.
- `elaboration/ElaborateConnection.hs:9:` Cannot connect ports $from -> $to as they have differing types: $fromType -> $toType.
  - Ports must have the same type when they are connected, this error is thrown when two ports are connected with different types.
- `elaboration/ElaborateConnection.hs:25:` Cannot find port with name $portName in $iostats
  - The port with name $portName was not found in the IO statements of an element. It prints the IO statements it did find.
- `elaboration/ElaborateConnection.hs:27:` Found several ports with name $portName in $iostats
  - Only place in Ex-PART that actually errors when several entities of the same names are found, instead of just picking the first one. There are several ports with the name $portName and there should not be. To help debugging, the IO statements that were searched were found.
- `elaboration/ElaborateConnection.hs:40:` Cannot find element with name $name in $elemnames
  - During Yosys postprocessing a bitwidth of ports must be found, and that can be quite hidden in the data structures. That's why these elements must be searched through and these kind of errors may be thrown. If an element that does not exist occurs in e.g. a connection statement this error may be thrown.
- `elaboration/ElaborateConnection.hs:42:` Found several components with name 
  - Similar issues but with several components with the name.
- `elaboration/Elaboration.hs:76:` Cannot find system $name in this scope. $sytem_names
  - Is thrown for system instantiations that refer to systems that are not in scope. Check if the system is in scope or you've given your instantiation/repetition statement the correct type. Also prints a list of names of systems it did find.
- `elaboration/Multiconnection.hs:8:` Cannot connect differing amount of ports: $from' -> $to'
  - Multiconnections can only connect ranges of the same size. Take care that the ranges you tried to connect are indeed of the same size. This can be especially obfuscated when not using the range operator.
- `elaboration/Multiconnection.hs:20:` Cannot find repetition with name \$repname for multiconnection \$repname:\$portname
  - The specified multiconnection refers to a repitition with $repname, the system did not find any repetition with that name in scope. Check if you are referring to the correct repetition.
- `elaboration/Repetition.hs:34:` Missing option `chain_in` in chain statement
  - Chain statements _must_ contain a `chain_in` option ([Chains](#chain-statement))
- `elaboration/Repetition.hs:37:` Missing option `chain_out` in chain statement
  - Chain statements _must_ contain a `chain_out` option ([Chains](#chain-statement))
- `elaboration/Repetition.hs:42:` Missing option `component` in repetition statement.
  - Repeat and chain statements _must_ contain a `component` option ([Chains](#chain-statement), [Repeat](#repeat-statement))
- `elaboration/Repetition.hs:45:` Missing option `amount` in a repetition statement.
  - Repeat and chain statements _must_ contain a `amount` option ([Chains](#chain-statement), [Repeat](#repeat-statement))
- `elaboration/Repetition.hs:48:` Missing option `layout` in a repetition statement.
  - Repeat and chain statements _must_ contain a `layout` option ([Chains](#chain-statement), [Repeat](#repeat-statement))
- `elaboration/Repetition.hs:119:` Cannot find element $elemName in source files.
  - The system searched for an element of a certain name but couldn't find either a component or a subsystem of that name. Check if you spelled the system or component name correctly in every repetition.
- `elaboration/Repetition.hs:172:` Unknown layout procedure.
  - The only available layout procedures are `horizontal`, `vertical`, and `identical` ([Repeat](#repeat-statement)). Use only those, or implement a new one at this line.
- `json-builder/Locations.hs:154:` Could not find ID $id in provided list.
  - The identifier $id in a coordinate or size expression could not be found. Make sure that the identifier was typed correctly and is indeed defined in the instantiation file.
- `json-builder/JSONBuilder.hs:21:` Top-level coordinates must be constants.
  - As defined in [System Definitions](#system-definitions), the top-level system must have constant coordinates. Ex-PART found a non-constant coordinate.
- `json-builder/JSONBuilder.hs:32:` expi file contains a cyclic coordinate definition, cannot generate location JSON.
  - Ex-PART found a cyclic dependency for coordinates or sizes, see issue [#13](https://github.com/PietPtr/Ex-PART/issues/13) for more information on seemingly resolvable dependencies.
- `nextpnr/Nextpnr.hs:28:` nextpnr terminated with code $code
  - Somewhere in nextpnr an error occured, usually this is a failed assertion, a segfault, or some error in the python script (`nextpnr/constrainer.py`). In any case, take a look at `nextpnr.err` for more information.
- `parser/Types.hs:149:` Cannot find bitwidth of type $type
  - Bitwidth for types are hardcoded in Ex-PART, as that was the fastest solution for now. It _should_ use Clash's system that maps types to bitwidths (See also issue [#2](https://github.com/PietPtr/Ex-PART/issues/2)). If you want to add a type's bitwidth, add it to the case statement here.
- `yosys/Postprocessing.hs:204:` Could not find driver $cid in $(sys_connections system)
  - $cid is some connection ID, so an element and a port. In the connections in the current system, no driver driving this connection ID was found. It also provides a list of connections of the system so you can see which connections _were_ found.
- `yosys/Postprocessing.hs:331:` cannot find net for cid in netmap \$cid (\$netmap)
  - Errors here are harder to debug and more often they are errors in Ex-PART and not in your code.
- `yosys/Postprocessing.hs:333:` No net found, something is disconnected... $port (\$relevantConnections) (\$netmap)
  - Some output port $port is not connected to anything. Much debug output is printed here as well, so you may miss the error because of all the extra output.
- `yosys/Preprocessing.hs:63:` Found zero-output component.
  - Components must have at least one output.
- `yosys/Yosys.hs:54:` Clash terminated with code $code
  - Clash terminated with an error. Take a look at the `clash.err` in one of the folders in `builds/` in the output directory.
- `yosys/Yosys.hs:138:` Yosys terminated with code $code
  - Yosys had some error, take a look at the `yosys.err` and `yosys.log` in the output directory.
