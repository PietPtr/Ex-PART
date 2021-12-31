# Ex-PART

*Ex*plicit *P*lace *A*nd *R*oute *T*ool: a low-level hardware description language for FPGA's allowing fine-grained control over placement of modules.

NOTE: this software was written for academic research, therefore no time was spent on a user-friendly interface, clear errors, or any other feature one may expect from maturely developed software. Your best hope of working with this project is opening it in GHCi and looking around the code when errors are encountered. Furthermore, much information is dumped in the output directory of the synthesized project. Errors not given by this tool may have ended up in a .err or .log file. 

## Getting Started

In `docs` you will find several guides on programming with Ex-PART. Ex-PART repurposes Clash's syntax for many of its constructs. Therefore it is good to be at least slightly familiar with Clash. Ex-PART is written completely in Haskell, given that Ex-PART is very much immature software, experience with reading Haskell code, and some understanding of its type system is strongly advised. Although the Haskell features used here are quite elementary, here are some subjects this project uses that you may need to brush up on: Parsec, Records.

In `docs/setting-up.md` you will find the nessecary versions of all the software Ex-PART depends on, and information on how to best structure and build an Ex-PART project.

In `examples/` several examples detailing all Ex-PART's features are located. The examples in the paper are `collatz`, `md5_reuse`, and `manycore`. Some explanations of what the examples are supposed to do can be found in `docs/examples.md`.

`docs/programming.md` is the guide for programming in Ex-PART. It contains documentation for every language construct, tips on how to best configure your editor, and warnings onn everything that seems like it should work but doesn't.

`docs/maintenance.md` is the Ex-PART maintenance manual. If you want to add a feature to Ex-PART, read this guide to discover where that can be done. It also contains an explanation of the project structure.

As Ex-PART was developed for a master thesis, there are still many issues with the software. Inspect the Github issues tab to find what bugs are present and which enhancements are possible.

In case the software does not run, one build directory (`collatz`) is committed to the repository, such that at least one run of building a project is available for inspection.

## Notes on the Paper

In the (as of yet unpublished) paper on Ex-PART, a hierarchy of component instantiations and connections is called a _module_. In the code and explanation in this repository it is called a _system_. The name module is used in the paper as it ought to call up the concept of Verilog modules, which are slightly more general than Ex-PART's systems.
