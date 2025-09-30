(* Define the n-ary tree structure
type 'a nary_tree = 
  | Leaf of 'a
  | Node of 'a * 'a nary_tree list
  (* | Node of 'a * ('a nary_tree list)ref  *)

(* Function to find the depth of the tree *)

let rec depth = function
| Leaf _ -> 1
| Node (_, children) -> 
    1 + (List.fold_left (fun acc child -> max acc (depth child)) 0 children)

let rec find_deepest_node tree current_depth target_depth =
  match tree with
  | Leaf _ -> 
      if current_depth = target_depth then Some tree else None
  | Node (_, children) ->
      if current_depth = target_depth then Some tree
      else
        List.fold_left (fun acc child ->
          match acc with
          | Some _ -> acc
          | None -> find_deepest_node child (current_depth + 1) target_depth
        ) None children

(* Function to find the deepest node and its path *)
let rec find_deepest_path tree current_depth target_depth path =
match tree with
| Leaf _ -> 
    if current_depth = target_depth then Some (List.rev (tree :: path)) else None
| Node (_, children) ->
    if current_depth = target_depth then Some (List.rev (tree :: path))
    else
      List.fold_left (fun acc child ->
        match acc with
        | Some _ -> acc
        | None -> find_deepest_path child (current_depth + 1) target_depth (tree :: path)
      ) None children


(* Function to rebuild the tree with the new node added to the deepest node *)
let rec rebuild_tree path new_node =
match path with
| [] -> failwith "Empty path"
| [Leaf value] -> Node (value, [new_node])
| [Node (value, children)] -> Node (value, children @ [new_node])
| Node (value, children) :: rest ->
    let updated_children = List.map (fun child ->
      if List.mem child rest then rebuild_tree rest new_node else child
    ) children in
    Node (value, updated_children)
| _ -> failwith "Invalid path"

(* Function to add a new node to the deepest node in the tree *)
let add_to_deepest tree new_node =
let tree_depth = depth tree in
match find_deepest_path tree 1 tree_depth [] with
| Some path -> rebuild_tree path new_node
| None -> tree (* This case should not happen if the tree is not empty *)


let example_tree = ref(
  Node (1, [
    Node (2, [
      Leaf(5);
      Leaf(6)
    ]);
    Node (3, [
      Node (7, [
        Leaf(8)
      ])
    ]);
    Leaf(4)
  ]))

let () = assert (depth !example_tree = 4);;
let () = assert (find_deepest_node !example_tree 1 4 = Some(Leaf(8)));;
let () = assert (add_to_deepest !example_tree (Leaf 9) = Node (1, [
                                                           Node (2, [
                                                             Leaf(5);
                                                             Leaf(6)
                                                           ]);
                                                           Node (3, [
                                                             Node (7, [
                                                               Node(8, [
                                                                 Leaf(9)
                                                               ])
                                                             ])
                                                           ]);
                                                           Leaf(4)
                                                         ]));;
example_tree := add_to_deepest !example_tree (Leaf 9);;
let () = assert (add_to_deepest !example_tree (Leaf 10) = Node (1, [
                                                           Node (2, [
                                                             Leaf(5);
                                                             Leaf(6)
                                                           ]);
                                                           Node (3, [
                                                             Node (7, [
                                                               Node(8, [
                                                                 Node(9, [
                                                                  Leaf(10)
                                                                 ])
                                                               ])
                                                             ])
                                                           ]);
                                                           Leaf(4)
                                                         ]));;

let example_tree_2 = ref(Leaf(1));;
let () = assert (depth !example_tree_2 = 1);;
let () = assert (find_deepest_node !example_tree_2 1 1 = Some(Leaf(1)));;
let () = assert (add_to_deepest !example_tree_2 (Leaf 2) = Node(1, [Leaf(2)]));;

let example_tree_3 = ref(Node(1, [Node(2, [Node(3,[Node(4,[Leaf(5)])])])]));;
let () = assert (depth !example_tree_3 = 5);;
let () = assert (find_deepest_node !example_tree_3 1 5 = Some(Leaf(5)));;
let () = assert (add_to_deepest !example_tree_3 (Leaf 6) = Node(1, [Node(2, [Node(3,[Node(4,[Node(5, [Leaf(6)])])])])]));;

(* ------------------------------ TEST WITH COMPLEX NODES ----------------------------*)

type block = {
  hash : string;
  epoch : int;
  slot : int;
  justified: bool;
  finalized : bool;
}

(* Define the n-ary tree structure *)
type 'a ethereum_tree = 
  | Leaf of block
  | Node of block * 'a ethereum_tree list

(* Function to find the depth of the tree *)
let rec depth = function
| Leaf _ -> 1
| Node (_, children) -> 
    1 + (List.fold_left (fun acc child -> max acc (depth child)) 0 children)

(* Helper function to get just the deepest node *)
let get_deepest_node tree =
  let rec aux t depth =
    match t with
    | Leaf _ -> (t, depth)
    | Node (_, children) ->
        List.fold_left (fun (best_node, best_depth) child ->
          let (child_node, child_depth) = aux child (depth + 1) in
          if child_depth > best_depth then (child_node, child_depth) else (best_node, best_depth)
        ) (t, depth) children
  in
  let (deepest_leaf, _) = aux tree 1 in
  deepest_leaf

  let rec find_deepest_path tree =
  match tree with
  | Leaf block -> [block]
  | Node (block, []) -> [block]
  | Node (block, children) ->
      let deepest_child_path = 
        List.fold_left (fun best_path child ->
          let child_path = find_deepest_path child in
          if List.length child_path > List.length best_path then child_path
          else best_path
        ) [] children
      in
      block :: deepest_child_path


(* Function to add a new node at the end of the deepest path *)
let rec add_to_deepest tree new_block =
  let rec aux t =
    match t with
    | Leaf block -> Node (block, [Leaf new_block])
    | Node (block, children) ->
        if children = [] then Node (block, [Leaf new_block])
        else
          (* Find the child with the deepest subtree *)
          let depths = List.map (fun c -> (c, depth c)) children in
          let (deepest_child, _) = List.fold_left (fun (best_c, best_d) (c, d) -> if d > best_d then (c, d) else (best_c, best_d)) (List.hd depths) depths in
          let updated_children = List.map (fun c -> if c == deepest_child then aux c else c) children in
          Node (block, updated_children)
  in
  aux tree

let deepest_justified_on_path tree =
  let path = find_deepest_path tree in
  List.fold_left (fun acc block ->
    if block.justified then Some block else acc
  ) None path

let get_last_four_checkpoints tree =
  let path = find_deepest_path tree in
  let root_block = match tree with Leaf b -> b | Node (b, _) -> b in
  (* For each epoch, keep the block with the lowest slot *)
  let epoch_map = List.fold_left (fun acc block ->
    match List.assoc_opt block.epoch acc with
    | None -> (block.epoch, block) :: acc
    | Some b -> if block.slot < b.slot then (block.epoch, block) :: List.remove_assoc block.epoch acc else acc
  ) [] path in
  (* Sort epochs descending, get blocks *)
  let checkpoints =
    epoch_map
    |> List.sort (fun (e1, _) (e2, _) -> compare e2 e1)
    |> List.map snd
  in
  let rec fill acc lst =
    match acc with
    | l when List.length l = 4 -> l
    | l -> (match lst with
            | [] -> fill (root_block :: l) []
            | hd :: tl -> fill (hd :: l) tl)
  in
  match fill [] checkpoints with
  | [a; b; c; d] -> (a, b, c, d)
  | _ -> assert false

let get_latest_checkpoint tree =
  let path = find_deepest_path tree in
  (* For each epoch, keep the block with the lowest slot *)
  let epoch_map = List.fold_left (fun acc block ->
    match List.assoc_opt block.epoch acc with
    | None -> (block.epoch, block) :: acc
    | Some b -> if block.slot < b.slot then (block.epoch, block) :: List.remove_assoc block.epoch acc else acc
  ) [] path in
  (* Find the checkpoint with the highest epoch *)
  match epoch_map with
  | [] -> None
  | _ ->
      let (_, latest_block) =
        List.fold_left (fun (max_epoch, max_block) (epoch, block) ->
          if epoch > max_epoch then (epoch, block) else (max_epoch, max_block)
        ) (fst (List.hd epoch_map), snd (List.hd epoch_map)) epoch_map
      in
      Some latest_block
  
let example_tree = ref(
  Node ({ hash = "leaf1"; epoch = 1; slot = 0; justified = true; finalized = true }, [
    Node ({ hash = "leaf2"; epoch = 2; slot = 2; justified = false; finalized = false }, [
      Leaf({ hash = "leaf5"; epoch = 3; slot = 12; justified = false; finalized = false });
      Leaf({ hash = "leaf6"; epoch = 3; slot = 30; justified = false; finalized = false })
    ]);
    Node ({ hash = "leaf3"; epoch = 2; slot = 12; justified = true; finalized = true }, [
      Node ({ hash = "leaf7"; epoch = 3; slot = 2; justified = true; finalized = false }, [
        Leaf({ hash = "leaf8"; epoch = 3; slot = 6; justified = false; finalized = false })
      ])
    ]);
    Leaf({ hash = "leaf4"; epoch = 2; slot = 9; justified = false; finalized = false })
  ]))

let () = assert (depth !example_tree = 4);;
let () = assert (get_deepest_node !example_tree = Leaf({ hash = "leaf8"; epoch = 3; slot = 6; justified = false; finalized = false }));;
let () = assert (add_to_deepest !example_tree { hash = "leaf9"; epoch = 5; slot = 10; justified = false; finalized = false } = 
                                                         Node ({ hash = "leaf1"; epoch = 1; slot = 0; justified = true; finalized = true }, [
                                                          Node ({ hash = "leaf2"; epoch = 2; slot = 2; justified = false; finalized = false }, [
                                                            Leaf({ hash = "leaf5"; epoch = 3; slot = 12; justified = false; finalized = false });
                                                            Leaf({ hash = "leaf6"; epoch = 3; slot = 30; justified = false; finalized = false })
                                                          ]);
                                                          Node ({ hash = "leaf3"; epoch = 2; slot = 12; justified = true; finalized = true }, [
                                                            Node ({ hash = "leaf7"; epoch = 3; slot = 2; justified = true; finalized = false }, [
                                                              Node({ hash = "leaf8"; epoch = 3; slot = 6; justified = false; finalized = false }, [
                                                                Leaf({ hash = "leaf9"; epoch = 5; slot = 10; justified = false; finalized = false })
                                                              ])
                                                            ])
                                                          ]);
                                                          Leaf({ hash = "leaf4"; epoch = 2; slot = 9; justified = false; finalized = false })
                                                        ]));;
example_tree := add_to_deepest !example_tree ({ hash = "leaf9"; epoch = 5; slot = 10; justified = false; finalized = false });;
let () = assert (add_to_deepest !example_tree ({ hash = "leaf10"; epoch = 6; slot = 14; justified = false; finalized = false }) = 
    Node ({ hash = "leaf1"; epoch = 1; slot = 0; justified = true; finalized = true }, [
                                                          Node ({ hash = "leaf2"; epoch = 2; slot = 2; justified = false; finalized = false }, [
                                                            Leaf({ hash = "leaf5"; epoch = 3; slot = 12; justified = false; finalized = false });
                                                            Leaf({ hash = "leaf6"; epoch = 3; slot = 30; justified = false; finalized = false })
                                                          ]);
                                                          Node ({ hash = "leaf3"; epoch = 2; slot = 12; justified = true; finalized = true }, [
                                                            Node ({ hash = "leaf7"; epoch = 3; slot = 2; justified = true; finalized = false }, [
                                                              Node({ hash = "leaf8"; epoch = 3; slot = 6; justified = false; finalized = false }, [
                                                                Node({ hash = "leaf9"; epoch = 5; slot = 10; justified = false; finalized = false }, [
                                                                  Leaf({ hash = "leaf10"; epoch = 6; slot = 14; justified = false; finalized = false })
                                                                ])
                                                              ])
                                                            ])
                                                          ]);
                                                          Leaf({ hash = "leaf4"; epoch = 2; slot = 9; justified = false; finalized = false })
                                                        ]));;
let () = assert (find_deepest_path !example_tree = [{hash = "leaf1"; epoch = 1; slot = 0; justified = true; finalized = true};
                                                    {hash = "leaf3"; epoch = 2; slot = 12; justified = true; finalized = true};
                                                    {hash = "leaf7"; epoch = 3; slot = 2; justified = true; finalized = false};
                                                    {hash = "leaf8"; epoch = 3; slot = 6; justified = false; finalized = false};
                                                    {hash = "leaf9"; epoch = 5; slot = 10; justified = false; finalized = false}]);;

let () = assert (deepest_justified_on_path !example_tree = Some {hash = "leaf7"; epoch = 3; slot = 2; justified = true; finalized = false});;
example_tree := add_to_deepest !example_tree ({ hash = "leaf10"; epoch = 5; slot = 18; justified = true; finalized = false });;
let () = assert (deepest_justified_on_path !example_tree = Some { hash = "leaf10"; epoch = 5; slot = 18; justified = true; finalized = false });;

let () = assert (get_last_four_checkpoints !example_tree = 
  ({ hash = "leaf1"; epoch = 1; slot = 0; justified = true; finalized = true },
   {hash = "leaf3"; epoch = 2; slot = 12; justified = true; finalized = true},
   {hash = "leaf7"; epoch = 3; slot = 2; justified = true; finalized = false},
   {hash = "leaf9"; epoch = 5; slot = 10; justified = false; finalized = false}));;

let example_tree2 = ref(
  Node ({ hash = "leaf1"; epoch = 1; slot = 0; justified = true; finalized = true }, [
    Leaf({ hash = "leaf2"; epoch = 2; slot = 2; justified = false; finalized = false });
    Leaf({ hash = "leaf3"; epoch = 2; slot = 12; justified = false; finalized = false });
    Leaf({ hash = "leaf4"; epoch = 2; slot = 14; justified = false; finalized = false });
    Leaf({ hash = "leaf5"; epoch = 2; slot = 17; justified = false; finalized = false });
    Leaf({ hash = "leaf6"; epoch = 2; slot = 20; justified = false; finalized = false });
  ]))

let () = assert (depth !example_tree2 = 2);;
let () = assert (get_deepest_node !example_tree2 = Leaf({ hash = "leaf2"; epoch = 2; slot = 2; justified = false; finalized = false }));;
let () = assert (add_to_deepest !example_tree2 { hash = "leaf7"; epoch = 3; slot = 1; justified = false; finalized = false } = 
                                                          Node ({ hash = "leaf1"; epoch = 1; slot = 0; justified = true; finalized = true }, [
                                                            Node({ hash = "leaf2"; epoch = 2; slot = 2; justified = false; finalized = false }, [
                                                              Leaf({ hash = "leaf7"; epoch = 3; slot = 1; justified = false; finalized = false })
                                                            ]);
                                                            Leaf({ hash = "leaf3"; epoch = 2; slot = 12; justified = false; finalized = false });
                                                            Leaf({ hash = "leaf4"; epoch = 2; slot = 14; justified = false; finalized = false });
                                                            Leaf({ hash = "leaf5"; epoch = 2; slot = 17; justified = false; finalized = false });
                                                            Leaf({ hash = "leaf6"; epoch = 2; slot = 20; justified = false; finalized = false });
                                                          ]));; *)