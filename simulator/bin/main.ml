
let () = 
  let t = Sys.time () in
  Implementation.Tenderbake.TenderbakeProtocol.run ();
  let elapsed_time = Sys.time () -. t in
  let s = Printf.sprintf "[DONE] Simulation completed in %.2f seconds." elapsed_time in
  print_endline s
