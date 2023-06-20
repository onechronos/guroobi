(** Conveniently set Gurobi params sourced from a JSON file *)

type t = {
  int_params : (string * int) list;
      (** association list consisting of Gurobi param names and their corresponding integer values *)
  float_params : (string * float) list;
      (** association list consisting of Gurobi param names and their corresponding float values *)
  string_params : (string * string) list;
      (** association list consisting of Gurobi param names and their corresponding string values *)
}

val read_and_set : ?path:string -> Gurobi.Raw.env -> (unit, string) result
(** [read_and_set gurobi_env] looks for the [GUROBI_PARAMS] Unix
    environment variable. If it finds it, it interprets it as the path
    of a JSON-formatted file. It attempts to read and parse the
    contents of that file. The following example illustrates its
    expected structure:
    {[ { 
         int_params : [["PARAM1", 3], ["PARAM2", 17]],
         string_params : [["ANOTHER_PARAM", "foo"]],
         float_params : [["ONE_FINAL_PARAM", 1e-42]]
       }
    ]}
    If then sets each of the parameter values onto the Gurobi
    environment [gurobi_env]. (The ordering of parameter setting is
    indeterminate.) [read_and_set] is intended to obviate the need for
    boilerplate code in setting standard Gurobi parameters.

    If an optional path is provided as an arguments,
    e.g. [read_and_set ~path:"/path/to/my-params.json" env], then
    parameter data is instead read from that file, rather than the
    file referenced by [GUROBI_PARAMS].
*)
