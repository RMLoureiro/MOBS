
let () = 
  print_endline "Starting simulation";
  Implementation.Bitcoin.BitcoinProtocol.run ();
  print_endline "Simulation completed"

