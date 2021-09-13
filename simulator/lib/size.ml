type t =
  | Bit of int
  | Byte of int
  | Kilobyte of int
  | Megabyte of int
  | Gigabyte of int


let to_bits (s:t) : int =
  match s with
  | Bit(n) -> n
  | Byte(n) -> 8*n
  | Kilobyte(n) -> 8192*n
  | Megabyte(n) -> 8388608*n
  | Gigabyte(n) -> 8589934592*n

