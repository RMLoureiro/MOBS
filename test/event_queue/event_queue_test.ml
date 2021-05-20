(* testing the getting and adding operations of the event queue *)
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
  Simulator.Event_queue.add_event(e1);
  Simulator.Event_queue.add_event(e2);
  Simulator.Event_queue.add_event(e3);
  Simulator.Event_queue.add_event(e4);
  Simulator.Event_queue.add_event(e5);
  Simulator.Event_queue.add_event(e6);
  Simulator.Event_queue.add_event(e7);
  Simulator.Event_queue.add_event(e8);
  Simulator.Event_queue.add_event(e9);
  Simulator.Event_queue.add_event(e10);
  Simulator.Event_queue.add_event(e11);
  Simulator.Event_queue.add_event(e12);
  let (_,ev1)  = Simulator.Event_queue.get_event () in assert(ev1  = e1);
  let (_,ev2)  = Simulator.Event_queue.get_event () in assert(ev2  = e2);
  let (_,ev3)  = Simulator.Event_queue.get_event () in assert(ev3  = e3);
  let (_,ev4)  = Simulator.Event_queue.get_event () in assert(ev4  = e4);
  let (_,ev5)  = Simulator.Event_queue.get_event () in assert(ev5  = e5);
  let (_,ev6)  = Simulator.Event_queue.get_event () in assert(ev6  = e6);
  let (_,ev7)  = Simulator.Event_queue.get_event () in assert(ev7  = e9);
  let (_,ev8)  = Simulator.Event_queue.get_event () in assert(ev8  = e7);
  let (_,ev9)  = Simulator.Event_queue.get_event () in assert(ev9  = e10);
  let (_,ev10) = Simulator.Event_queue.get_event () in assert(ev10 = e11);
  let (_,ev11) = Simulator.Event_queue.get_event () in assert(ev11 = e8);
  let (_,ev12) = Simulator.Event_queue.get_event () in assert(ev12 = e12);
  print_endline "ALL ASSERTIONS PASSED!"