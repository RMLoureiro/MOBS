(* testing the logging operations *)
let e1 = Simulator.Events.AddNode (0, 1)
let e2 = Simulator.Events.AddNode (1, 1)
let e3 = Simulator.Events.AddNode (2, 2)
let e4 = Simulator.Events.AddLink (0, 1)
let e5 = Simulator.Events.AddLink (0, 2)
let e6 = Simulator.Events.AddLink (1, 2)
let e7 = Simulator.Events.Message (0, 1, 21, Simulator.Messages.Message "Hi1")
let e8 = Simulator.Events.Message (0, 1, 27, Simulator.Messages.Message "Hi2")
let e9 = Simulator.Events.Message (1, 2, 16, Simulator.Messages.Message "Hi3")
let e10 = Simulator.Events.Message (2, 0, 23, Simulator.Messages.Message "Hi4")
let e11 = Simulator.Events.Timeout (1, 26, "Label1")
let e12 = Simulator.Events.Timeout (2, 37, "Label2")

let () = 
  Simulator.Logging.init ();
  Simulator.Logging.log_event e1;
  Simulator.Logging.log_event e2;
  Simulator.Logging.log_event e3;
  Simulator.Logging.log_event e4;
  Simulator.Logging.log_event e5;
  Simulator.Logging.log_event e6;
  Simulator.Logging.log_event e7;
  Simulator.Logging.log_event e8;
  Simulator.Logging.log_event e9;
  Simulator.Logging.log_event e10;
  Simulator.Logging.log_event e11;
  Simulator.Logging.log_event e12;
  Simulator.Logging.terminate ()