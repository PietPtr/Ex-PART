# Ex-PART

*Ex*plicit *P*lace *A*nd *R*oute *T*ool: a low-level hardware description language for FPGA's allowing fine-grained control over placement of modules.

NOTE: this software was written for academic research, therefore no time was spent on a user-friendly interface, clear errors, or any other feature one may expect from maturely developed software. Your best hope of working with this project is opening it in GHCi and looking around the code when errors are encountered. Furthermore, much information is dumped in the output directory of the synthesized project. Errors not given by this tool may have ended up in a .err or .log file. 

## Getting Started

In `docs/setting-up.md` you will find the nessecary versions of all the software Ex-PART depends on, and information on how to best structure and build an Ex-PART project.

In `examples/` several examples detailing all Ex-PART's features are located. The examples in the paper are `collatz`, `md5_reuse`, and `manycore`.

`docs/programming.md` is the guide for programming in Ex-PART. It contains documentation for every language construct, tips on how to best configure your editor, and warnings onn everything that seems like it should work but doesn't.

`docs/maintenance.md` is the Ex-PART maintenance manual. If you want to add a feature to Ex-PART, read this guide to discover where that can be done. It also contains an explanation of the project structure.

As Ex-PART was developed for a master thesis, there are still many issues with the software. Inspect the Github issues tab to discover what bugs are present and which enhancements are possible.