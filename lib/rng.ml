(* TODO : needs to be changed *)
(* https://github.com/lucasaiu/ocaml/blob/master/stdlib/random.ml *)

(* regardless of the arbitrary number of args in <seed_args>, all numbers generated will follow the global seed *)
(*
let get_int (seed_args:int array) = 
  let global_seed = Array.make 1 !Parameters.General.seed in
  Random.full_init (Array.append global_seed seed_args);
  Random.int !Parameters.General.num_nodes
*)


(* TODO : in the future, this class' behavior might be moved to the abstractions 
    for committee selection, for example
*)



