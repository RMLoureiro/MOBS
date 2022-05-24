

let () = 
  let t = Sys.time () in
  Random.init !Parameters.General.seed;
  let seeds_per_batch = Array.init !Parameters.General.num_batches (fun _ -> Random.int 999999) in
  while !Parameters.General.current_batch <= !Parameters.General.num_batches do
    let cur_batch = !Parameters.General.current_batch in
    print_endline (Printf.sprintf "BATCH %d of %d" cur_batch !Parameters.General.num_batches);
    Random.init seeds_per_batch.(cur_batch-1);
    ( (* Must extend when adding new protocols to the simulator *)
      match !Parameters.General.protocol with
      | "algorand" -> Algorand.AlgorandProtocol.run()
      | "bitcoin" -> Bitcoin.BitcoinProtocol.run()
      | "example" -> Simplepow.SimpleProtocol.run()
      | "ouroboros-bft" -> BftED.CardanoProtocol.run()
      | "ouroboros-praos" -> PraosED.CardanoProtocol.run()
      | "tenderbake" -> Tenderbake.TenderbakeProtocol.run()
      | _ -> (print_endline (Printf.sprintf "Unrecognized protocol <%s>" !Parameters.General.protocol); exit 0)
    );
    Parameters.General.current_batch := cur_batch + 1;
  done;
  let elapsed_time = Sys.time () -. t in
  let s = Printf.sprintf "[DONE] Simulation completed in %.2f seconds." elapsed_time in
  print_endline s
