(** Wrapper for a value. *)
module type T = sig
  type v
end

(** Signature of the unique module. Purpose is to get a unique identifier for any value of type {b v}. *)
module type Unique = sig
  (** Type of the values being converted to unique integers. *)
  type v

  (** Get an integer that uniquely identifies {b v}.
    @param x the value for which we want a unique identifier
  *)
  val id : v -> int
end

(** Create an implementation for Unique, given a wrapper {b T} for a type of values. *)
module Make(T:T):(Unique with type v = T.v) = struct

  type v = T.v

  let store : v list ref = ref [] (* TODO : use hashtable for faster lookups *)

  let id (v:v) =
    let rec find y l i =
      match l with
      | [] -> None
      | x::xs -> if x = y then Some(i) else find y xs (i+1)
    in
    let res = find v !store 1 in
    match res with
    | Some(index) -> index
    | None -> 
      begin
        store := !store@[v];
        List.length !store
      end

end