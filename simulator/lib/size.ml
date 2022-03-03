(** Type of sizes supported. *)
type t =
  | Bit of int
  | Byte of int
  | Kilobyte of int
  | Megabyte of int
  | Gigabyte of int

(** Convert a given size to bits. *)
let to_bits (s:t) : int =
  match s with
  | Bit(n) -> n
  | Byte(n) -> 8*n
  | Kilobyte(n) -> 8192*n
  | Megabyte(n) -> 8388608*n
  | Gigabyte(n) -> 8589934592*n

(** Convert a given size to megabytes. *)
let to_megabytes (s:t) : float =
  match s with
  | Bit(n) -> (float_of_int n) /. 8388608.0
  | Byte(n) -> (float_of_int n) /. 1048576.0
  | Kilobyte(n) -> (float_of_int n) /. 1024.0
  | Megabyte(n) -> (float_of_int n)
  | Gigabyte(n) -> (float_of_int n) *. 1024.0

