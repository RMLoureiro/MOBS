
let () = 
  let t = Sys.time () in
  Implementation.Algorand.AlgorandProtocol.run ();
  let elapsed_time = Sys.time () -. t in
  Printf.printf "[DONE] Simulation completed in %.2f seconds. \n" elapsed_time
