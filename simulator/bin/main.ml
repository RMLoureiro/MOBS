
let () = 
  let t = Sys.time () in
  print_endline "\t Running simulation...";
  Implementation.Algorand.AlgorandProtocol.run ();
  let elapsed_time = Sys.time () -. t in
  Printf.printf "\t Simulation completed in %.2f seconds. \n" elapsed_time
