type key = {
  name : string;
  app_name : string;
  expiration : int;
  v : string;
}

val get : string -> (key, string) result
val set : Raw.env -> string -> (unit, string) result
