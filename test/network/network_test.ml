let () = 
  let regions = Abstractions.Network.node_regions () in
  let links   = Abstractions.Network.node_links () in
  print_string "Node Regions Assigned: [";
  List.iter (fun x -> print_int x; print_string ", ") regions;
  print_endline "]";
  print_endline "Node Links Assigned: [";
  List.iter (fun x -> print_string "["; (List.iter (fun y -> print_int y; print_string ", ") x); print_endline "],") links;
  print_endline "]"