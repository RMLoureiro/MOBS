(* the base message type - new messages will "extend" this type *)
(* TODO *)
type message =
  Message of string

  (* messages MUST contain a creator and a creation timestamp *)

let msg_to_json (msg:message) =
  match msg with
  | _ -> "{}"