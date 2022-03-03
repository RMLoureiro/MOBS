(** Keep track of statistics. *)
module type Stats = sig

  (** The type of values being processed. *)
  type t

  (** A {b node_id} has produced/seen a {b value}. *)
  val process : int -> t -> unit

  (** Returns a JSON string containing the statistics and respective values. *)
  val get : unit -> string

  (** Reset statistics data. *)
  val clear : unit -> unit

end

module Make = struct

  module type Arg = sig

    (** label for the metric *)
    val label         : string

    (** true considers the values to be timestamps and computes the average interval between "process" calls *)
    val use_intervals : bool

    (** the format to be used ({b format} : 0 = no format) ({b format} : 1 = convert ms to s, when dealing with times) *)
    val format        : int 
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

    let percentile95 lst =
      let index = int_of_float(floor(float_of_int(List.length lst) *. 0.95)) in
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
          let last_value = last_obs_value_per_node.(node) in
          obs_values_per_node.(node) <- [value-last_value]@(obs_values_per_node.(node));
          last_obs_value_per_node.(node) <- value
        )
      else
        obs_values_per_node.(node) <- [value]@(obs_values_per_node.(node))
  end

  (** Create an implementation for a Statistics module that computes an Average. *)
  module Average(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let last_obs_value_per_node : t array  = Array.init !Parameters.General.num_nodes (fun _ -> 0)
    let obs_values_per_node : t list array = Array.init !Parameters.General.num_nodes (fun _ -> [])

    let process (node:int) (value:t) =
      Aux.process node value last_obs_value_per_node obs_values_per_node X.use_intervals

    let get () =
      let res = Aux.average (Aux.per_node_metric obs_values_per_node Aux.average) in
      let resf  = if X.format = 1 then Aux.to_seconds res else string_of_int res in
      Printf.sprintf "{\"%s\":%s}" X.label resf

    let clear () =
      Array.fill last_obs_value_per_node 0 (!Parameters.General.num_nodes) 0;
      Array.fill obs_values_per_node 0 (!Parameters.General.num_nodes) []

  end

  (** Create an implementation for a Statistics module that computes a Median. *)
  module Median(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let last_obs_value_per_node : t array  = Array.init !Parameters.General.num_nodes (fun _ -> 0)
    let obs_values_per_node : t list array = Array.init !Parameters.General.num_nodes (fun _ -> [])

    let process (node:int) (value:t) =
      Aux.process node value last_obs_value_per_node obs_values_per_node X.use_intervals

    let get () =
      let res = Aux.median (Aux.per_node_metric obs_values_per_node Aux.median) in
      let resf  = if X.format = 1 then Aux.to_seconds res else string_of_int res in
      Printf.sprintf "{\"%s\":%s}" X.label resf

    let clear () =
      Array.fill last_obs_value_per_node 0 (!Parameters.General.num_nodes) 0;
      Array.fill obs_values_per_node 0 (!Parameters.General.num_nodes) []
    
  end

  (** Create an implementation for a Statistics module that computes a 95th Percentile. *)
  module Percentile95(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let last_obs_value_per_node : t array  = Array.init !Parameters.General.num_nodes (fun _ -> 0)
    let obs_values_per_node : t list array = Array.init !Parameters.General.num_nodes (fun _ -> [])

    let process (node:int) (value:t) =
      Aux.process node value last_obs_value_per_node obs_values_per_node X.use_intervals

    let get () =
      let res = Aux.percentile95 (Aux.per_node_metric obs_values_per_node Aux.percentile95) in
      let resf  = if X.format = 1 then Aux.to_seconds res else string_of_int res in
      Printf.sprintf "{\"%s\":%s}" X.label resf

    let clear () =
      Array.fill last_obs_value_per_node 0 (!Parameters.General.num_nodes) 0;
      Array.fill obs_values_per_node 0 (!Parameters.General.num_nodes) []
    
  end

  (** Create an implementation for a Statistics module that computes a Maximum. *)
  module Max(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let last_obs_value_per_node : t array  = Array.init !Parameters.General.num_nodes (fun _ -> 0)
    let obs_values_per_node : t list array = Array.init !Parameters.General.num_nodes (fun _ -> [])

    let process (node:int) (value:t) =
      Aux.process node value last_obs_value_per_node obs_values_per_node X.use_intervals

    let get () =
      let res = Aux.max (Aux.per_node_metric obs_values_per_node Aux.max) in
      let resf  = if X.format = 1 then Aux.to_seconds res else string_of_int res in
      Printf.sprintf "{\"%s\":%s}" X.label resf

    let clear () =
      Array.fill last_obs_value_per_node 0 (!Parameters.General.num_nodes) 0;
      Array.fill obs_values_per_node 0 (!Parameters.General.num_nodes) []
      
  end

  (** Create an implementation for a Statistics module that computes a Minimum. *)
  module Min(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let last_obs_value_per_node : t array  = Array.init !Parameters.General.num_nodes (fun _ -> 0)
    let obs_values_per_node : t list array = Array.init !Parameters.General.num_nodes (fun _ -> [])

    let process (node:int) (value:t) =
      Aux.process node value last_obs_value_per_node obs_values_per_node X.use_intervals

    let get () =
      let res = Aux.min (Aux.per_node_metric obs_values_per_node Aux.min) in
      let resf  = if X.format = 1 then Aux.to_seconds res else string_of_int res in
      Printf.sprintf "{\"%s\":%s}" X.label resf

    let clear () =
      Array.fill last_obs_value_per_node 0 (!Parameters.General.num_nodes) 0;
      Array.fill obs_values_per_node 0 (!Parameters.General.num_nodes) []

  end

  (** Create an implementation for a Statistics module that computes a Counter per Node. *)
  module CountPerNode(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let c = Array.init (!Parameters.General.num_nodes) (fun _ -> 0) 

    let process (node:int) (num:t) =
      c.(node) <- c.(node) + num

    let get _ =
      let rec arr_to_str str arr =
        match arr with
        | [] -> Printf.sprintf "%s]" str
        | x::[] -> arr_to_str (Printf.sprintf "%s%d" str x) []
        | x::xs -> arr_to_str (Printf.sprintf "%s%d," str x) xs
      in
      let res = arr_to_str "[" (Array.to_list c) in
      Printf.sprintf "{\"%s\":%s}" X.label res

    let clear () =
      Array.fill c 0 (!Parameters.General.num_nodes) 0

  end

  (** Create an implementation for a Statistics module that computes a Global Counter for integers. *)
  module CountAll(X:Arg) : (Stats with type t = int) = struct
    type t = int

    let c = ref 0

    let process (_:int) (num:t) =
      c := !c + num

    let get _ =
      let res = string_of_int (!c) in
      Printf.sprintf "{\"%s\":%s}" X.label res

    let clear () =
      c := 0

  end

  (** Create an implementation for a Statistics module that computes a Global Counter for floats. *)
  module CountAllF(X:Arg) : (Stats with type t = float) = struct
    type t = float

    let c = ref 0.0

    let process (_:int) (num:t) =
      c := !c +. num

    let get _ =
      let res = Printf.sprintf "%.2f" (!c) in 
      Printf.sprintf "{\"%s\":%s}" X.label res

    let clear () =
      c := 0.0

  end

end

(** Module for gathering {b NO} statistics. *)
module Empty = struct 
  type t = int

  let process _ _ =
    ()

  let get _ =
    "{}"

  let clear _ =
    ()

end

(** Functor that composes two statistic modules ({b A} and {b B}) into a single Statistics module. *)
module Compose(A:Stats)(B:Stats) : Stats = struct
  
  type t = int


  let process _ _ =
    ()

  let get () = 
    let s1 = A.get () in
    let s2 = B.get () in
    let sub1 = String.sub s1 0 ((String.length s1) -1) in
    let sub2 = String.sub s2 1 ((String.length s2) -1) in
    if String.length sub2 > 1 then
      Printf.sprintf "%s,%s" sub1 sub2
    else
      Printf.sprintf "%s%s" sub1 sub2

  let clear () =
    A.clear (); B.clear ()

end