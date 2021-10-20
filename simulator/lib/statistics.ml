module type Stats = sig

  (** the type of values being processed *)
  type t

  (** node_id <int> has produced value <t>*)
  val process : int -> t -> unit

  (** returns a JSON string containing the statistics and respective values *)
  (* <int> format : 0 = ms; 1 = s *)
  val get : int -> string

end

module Make = struct

  module type Arg = sig
    val label         : string (* label for the metric *)
    val use_intervals : bool   (* true considers the values to be timestamps and computes the average interval between "process" calls *)
  end

  module Aux = struct
    let min lst =
      let index = 0 in
      List.nth (List.sort compare lst) index
    
    let max lst =
      let index = (List.length lst) - 1 in
      List.nth (List.sort compare lst) index

    let median lst =
      let index = (List.length lst) / 2 in
      List.nth (List.sort compare lst) index

    let average lst =
      let sum = ref 0 in
      List.iter (fun y -> sum := !sum + y) lst;
      if List.length lst > 0 then
        !sum / (List.length lst)
      else
        0

    let per_node_metric lst operation = 
      let res = ref [] in
      Array.iter 
      (
        fun x ->
          let avg = operation x in
          res := [avg]@(!res)
      ) lst;
      !res

    let to_seconds ms =
      Printf.sprintf "%.2f" ((float_of_int ms) /. 1000.0)

    let process node value last_obs_value_per_node obs_values_per_node use_intervals =
      if use_intervals then
        (
          let last_value = last_obs_value_per_node.(node-1) in
          obs_values_per_node.(node-1) <- [value-last_value]@(obs_values_per_node.(node-1));
          last_obs_value_per_node.(node-1) <- value
        )
      else
        obs_values_per_node.(node-1) <- [value]@(obs_values_per_node.(node-1))
  end

  module Average(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let last_obs_value_per_node : t array  = Array.init !Parameters.General.num_nodes (fun _ -> 0)
    let obs_values_per_node : t list array = Array.init !Parameters.General.num_nodes (fun _ -> [])

    let process (node:int) (value:t) =
      Aux.process node value last_obs_value_per_node obs_values_per_node X.use_intervals

    let get format =
      let res_ms = Aux.average (Aux.per_node_metric obs_values_per_node Aux.average) in
      let res_s  = Aux.to_seconds res_ms in
      let res = if format = 1 then res_s else string_of_int res_ms in
      Printf.sprintf "{\"%s\":%s}" X.label res
  end

  module Median(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let last_obs_value_per_node : t array  = Array.init !Parameters.General.num_nodes (fun _ -> 0)
    let obs_values_per_node : t list array = Array.init !Parameters.General.num_nodes (fun _ -> [])

    let process (node:int) (value:t) =
      Aux.process node value last_obs_value_per_node obs_values_per_node X.use_intervals

    let get format =
      let res_ms = Aux.median (Aux.per_node_metric obs_values_per_node Aux.median) in
      let res_s  = Aux.to_seconds res_ms in
      let res = if format = 1 then res_s else string_of_int res_ms in
      Printf.sprintf "{\"%s\":%s}" X.label res
  end

  module Max(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let last_obs_value_per_node : t array  = Array.init !Parameters.General.num_nodes (fun _ -> 0)
    let obs_values_per_node : t list array = Array.init !Parameters.General.num_nodes (fun _ -> [])

    let process (node:int) (value:t) =
      Aux.process node value last_obs_value_per_node obs_values_per_node X.use_intervals

    let get format =
      let res_ms = Aux.max (Aux.per_node_metric obs_values_per_node Aux.max) in
      let res_s  = Aux.to_seconds res_ms in
      let res = if format = 1 then res_s else string_of_int res_ms in
      Printf.sprintf "{\"%s\":%s}" X.label res
  end

  module Min(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let last_obs_value_per_node : t array  = Array.init !Parameters.General.num_nodes (fun _ -> 0)
    let obs_values_per_node : t list array = Array.init !Parameters.General.num_nodes (fun _ -> [])

    let process (node:int) (value:t) =
      Aux.process node value last_obs_value_per_node obs_values_per_node X.use_intervals

    let get format =
      let res_ms = Aux.min (Aux.per_node_metric obs_values_per_node Aux.min) in
      let res_s  = Aux.to_seconds res_ms in
      let res = if format = 1 then res_s else string_of_int res_ms in
      Printf.sprintf "{\"%s\":%s}" X.label res
  end

end

module Empty = struct 
  type t = int

  let process _ _ =
    ()

  let get _ =
    "{}"
end

module Compose(A:Stats)(B:Stats) : Stats = struct
  
  type t = int


  let process _ _ =
    ()

  let get format = 
    let s1 = A.get format in
    let s2 = B.get format in
    let sub1 = String.sub s1 0 ((String.length s1) -1) in
    let sub2 = String.sub s2 1 ((String.length s2) -1) in
    if String.length sub2 > 1 then
      Printf.sprintf "%s,%s" sub1 sub2
    else
      Printf.sprintf "%s%s" sub1 sub2

end