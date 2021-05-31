module Msg = struct
  type t = string

  let to_json (m: t) =
    String.concat "" ["{\"string\":\"";m;"\"}"]
end

module TestEvent = Simulator.Events.MakeEvent(Msg);;
module TestQueue = Simulator.Events.MakeQueue(TestEvent);;

(* testing the getting and adding operations of the event queue *)
let e1  = TestEvent.AddNode (0, 1)
let e2  = TestEvent.AddNode (1, 1)
let e3  = TestEvent.AddNode (2, 2)
let e4  = TestEvent.AddLink (0, 1)
let e5  = TestEvent.AddLink (0, 2)
let e6  = TestEvent.AddLink (1, 2)
let e7  = TestEvent.Message (0, 1, 21, "Hi1")
let e8  = TestEvent.Message (0, 1, 27, "Hi2")
let e9  = TestEvent.Message (1, 2, 16, "Hi3")
let e10 = TestEvent.Message (2, 0, 23, "Hi4")
let e11 = TestEvent.Timeout (1, 26, "Label1")
let e12 = TestEvent.Timeout (2, 37, "Label2")

let () = 
  TestQueue.add_event(e1);
  TestQueue.add_event(e2);
  TestQueue.add_event(e3);
  TestQueue.add_event(e4);
  TestQueue.add_event(e5);
  TestQueue.add_event(e6);
  TestQueue.add_event(e7);
  TestQueue.add_event(e8);
  TestQueue.add_event(e9);
  TestQueue.add_event(e10);
  TestQueue.add_event(e11);
  TestQueue.add_event(e12);
  let (_,ev1)  = TestQueue.get_event () in assert(ev1  = e1);
  let (_,ev2)  = TestQueue.get_event () in assert(ev2  = e2);
  let (_,ev3)  = TestQueue.get_event () in assert(ev3  = e3);
  let (_,ev4)  = TestQueue.get_event () in assert(ev4  = e4);
  let (_,ev5)  = TestQueue.get_event () in assert(ev5  = e5);
  let (_,ev6)  = TestQueue.get_event () in assert(ev6  = e6);
  let (_,ev7)  = TestQueue.get_event () in assert(ev7  = e9);
  let (_,ev8)  = TestQueue.get_event () in assert(ev8  = e7);
  let (_,ev9)  = TestQueue.get_event () in assert(ev9  = e10);
  let (_,ev10) = TestQueue.get_event () in assert(ev10 = e11);
  let (_,ev11) = TestQueue.get_event () in assert(ev11 = e8);
  let (_,ev12) = TestQueue.get_event () in assert(ev12 = e12);
  print_endline "ALL ASSERTIONS PASSED!"