module Msg = struct
  type t = string

  let to_json (m: t) =
    String.concat "" ["{\"string\":\"";m;"\"}"]
end

module TestEvent  = Simulator.Events.MakeEvent(Msg);;
module TestLogger = Simulator.Logging.Make(Msg)(TestEvent);;

(* testing the logging operations *)
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
  TestLogger.init ();
  TestLogger.log_event e1;
  TestLogger.log_event e2;
  TestLogger.log_event e3;
  TestLogger.log_event e4;
  TestLogger.log_event e5;
  TestLogger.log_event e6;
  TestLogger.log_event e7;
  TestLogger.log_event e8;
  TestLogger.log_event e9;
  TestLogger.log_event e10;
  TestLogger.log_event e11;
  TestLogger.log_event e12;
  TestLogger.terminate ()