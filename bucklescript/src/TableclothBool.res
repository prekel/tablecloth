type t = bool

let fromInt = i =>
  switch i {
  | 0 => Some(false)
  | 1 => Some(true)
  | _ => None
  }

let from_int = fromInt

let fromString = string =>
  switch string {
  | "false" => Some(false)
  | "true" => Some(true)
  | _ => None
  }

let from_string = fromString

external \"&&": (bool, bool) => bool = "%sequand"

external \"||": (bool, bool) => bool = "%sequor"

let xor = (a, b) => (a && !b) || (!a && b)

let not = not

@send external toString: bool => string = "toString"

let to_string = toString

let toInt = t => t ? 1 : 0

let to_int = toInt

let compare = compare

let equal = \"="
