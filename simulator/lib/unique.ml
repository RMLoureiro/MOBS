module type T = sig
  type v
end

module type Unique = sig
  type v
  val id : v -> int
end

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