

let get_protocol_parameter json param =
  let open Yojson.Basic.Util in
  json |> member "protocol" |> member param

let get_int_parameter param =
  let json = Yojson.Basic.from_file !General.parameters_file in
  let open Yojson.Basic.Util in
  get_protocol_parameter json param |> to_int

let get_float_parameter param =
  let json = Yojson.Basic.from_file !General.parameters_file in
  let open Yojson.Basic.Util in
  get_protocol_parameter json param |> to_float

let get_string_parameter param =
  let json = Yojson.Basic.from_file !General.parameters_file in
  let open Yojson.Basic.Util in
  get_protocol_parameter json param |> to_string