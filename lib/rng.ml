(* makes use of the Random module (https://ocaml.org/api/Random.html) *)

(* regardless of the arbitrary number of args in <seed_args>, all numbers generated will follow the global seed *)
let get_int (seed_args:int array) = 
  let global_seed = Array.make 1 !Parameters.Sim_parameters.seed in
  Random.full_init (Array.append global_seed seed_args);
  Random.int !Parameters.Sim_parameters.num_nodes

(* TODO : in OCaml we cannot have multiple instances of Random using different seeds
    since calling Random.init changes the seed, when going back to the previous seed the 
    numbers generated will repeat. Think about whether or not this is a problem.
    -> at least for the way rng was used on Algorand in the other simulator, this would
       not be an issue
    -> need to think about other use cases
*)

(* TODO : in the future, this class' behavior might be moved to the abstractions 
    for committee selection, for example
*)



