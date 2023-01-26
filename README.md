An OCaml wrapper to the Gurobi optimization library.
[Gurobi](https://www.gurobi.com)'s, specifically of its [C
API](https://www.gurobi.com/documentation/10.0/refman/c_api_overview.html).

OCaml wrappers to Gurobi exist:
[ocaml-gurobi](https://github.com/zhelih/ocaml-gurobi),
[ocaml-lp](https://github.com/ktahar/ocaml-lp/tree/master/src/lp-gurobi). Unlike
these, [guroobi](https://github.com/onechronos/guroobi) is *not* based
on [ocaml-ctypes](https://github.com/yallop/ocaml-ctypes). The
verbosity emanating from this design decision is well worth the extra
flexibility.
