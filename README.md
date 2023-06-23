# Guroobi

[guroobi](https://github.com/onechronos/guroobi) is an OCaml wrapper
to the commercial [Gurobi](https://www.gurobi.com) optimization
library, specifically of its [C
API](https://www.gurobi.com/documentation/10.0/refman/c_api_overview.html). 
[guroobi](https://github.com/onechronos/guroobi)
was developed and tested with [Gurobi](https://www.gurobi.com) version
10.

# Installation

To build [guroobi](https://github.com/onechronos/guroobi), first
download and install [Gurobi](https://www.gurobi.com); this will
require a license, as [Gurobi](https://www.gurobi.com) is
commercial software. Then, clone the
[guroobi](https://github.com/onechronos/guroobi) repository:
```sh
git clone https://github.com/onechronos/guroobi.git 
cd guroobi
```
Next, set two environment variables necessary to build
[guroobi](https://github.com/onechronos/guroobi) and run unit tests:
```sh
export GUROBI_ROOT=/path/to/gurobi 
export LD_LIBRARY_PATH=$GUROBI_ROOT/lib
```
Here, `/path/to/gurobi` must be replaced with the path of the
directory where [Gurobi](https://www.gurobi.com) architecture-specific files live. Finally, 
build and install:
```sh
opam pin add .
```
or
```sh
dune build
dune install
```

# Examples

[guroobi](https://github.com/onechronos/guroobi)'s [unit
tests](https://github.com/onechronos/guroobi/tree/master/test) offer
some examples of [guroobi](https://github.com/onechronos/guroobi) in
action. They consist of OCaml translations of some of
[Gurobi](https://www.gurobi.com)'s [C
examples](https://www.gurobi.com/documentation/10.0/examples/c_examples.html).


# Related

Other OCaml wrappers to [Gurobi](https://www.gurobi.com) exist:
[ocaml-gurobi](https://github.com/zhelih/ocaml-gurobi),
[ocaml-lp](https://github.com/ktahar/ocaml-lp/tree/master/src/lp-gurobi). Unlike
these, [guroobi](https://github.com/onechronos/guroobi) is *not* based
on [ocaml-ctypes](https://github.com/yallop/ocaml-ctypes). In the
author's opinion, the verbosity emanating from this design decision is
worth the extra flexibility.
