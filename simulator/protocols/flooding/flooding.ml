open Implementation

type msg =
  |M of int 


module FloodingMsg : (Simulator.Events.Message with type t = msg) = struct
  type t = msg

  let to_json (m:t) : string =
    match m with
    |M( vote) ->Printf.sprintf "{\"vote\":\"%d\"}" vote

  let get_size (_:t) =
    Simulator.Size.Kilobyte(534)

  let processing_time (_:t) = 2

  let identifier (msg:t) = match msg with
    |M( vote) -> vote

end

module FloodingEvent   = Simulator.Events.MakeEvent(FloodingMsg);;
module FloodingQueue   = Simulator.Events.MakeQueue(FloodingEvent);;
module FloodingNetwork = Abstractions.Network.Make(FloodingEvent)(FloodingQueue)(FloodingMsg);;
module FloodingLogger  = Simulator.Logging.Make(FloodingMsg)(FloodingEvent);;
module FloodingTimer   = Abstractions.Timer.Make(FloodingEvent)(FloodingQueue);;

module FloodingNode : (Protocol.AbstractNode with type ev=FloodingEvent.t and type value = string) = struct
  type value = string

  module V = struct
  type v = value
  end

  include Protocol.MakeBaseNode(V)

  type ev = FloodingEvent.t

  type node_data = None

  type t = (node_data, value) Protocol.template

  let init id links region : (t) =
    {
    id = id;
    region = region;
    links = links;
    state = "IDLE";
    data = None
    }

  let handle (node:t) (event:ev) : t =
    (match event with
      | FloodingEvent.Message(_,_,_,msg) ->
      match msg with
        |M(vote) as m ->
        if node.state = "IDLE" then
          let () = node.state <- "DONE" in
          FloodingNetwork.send_to_neighbors node.id m
    );
    node

end

module FloodingInitializer : (Protocol.Initializer with type node=FloodingNode.t and type ev=FloodingEvent.t) = struct

  type node = FloodingNode.t

  type ev = FloodingEvent.t

  let init nodes =
    let index = (Random.int ((Hashtbl.length nodes))) in
    [FloodingEvent.Message(index,index,0,M(index))]

end

module FloodingProtocol = Protocol.Make.Abstract(FloodingEvent)(FloodingQueue)(FloodingTimer)(FloodingNode)(FloodingNode)(FloodingInitializer)(FloodingLogger)(Simulator.Statistics.Empty)(FloodingNetwork);;