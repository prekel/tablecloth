type t<'a> = option<'a>

let some = a => Some(a)

let isSome = Belt.Option.isSome

let is_some = isSome

let isNone = Belt.Option.isNone

let is_none = isNone

let or_ = (ta, tb) => isSome(ta) ? ta : tb

let orElse = (ta, tb) => isSome(tb) ? tb : ta

let or_else = orElse

let and_ = (ta, tb) => isSome(ta) ? tb : ta

let andThen = (t, ~f) =>
  switch t {
  | None => None
  | Some(x) => f(x)
  }

let and_then = andThen

let flatten = x =>
  switch x {
  | Some(option) => option
  | None => None
  }

let both = (a, b) =>
  switch (a, b) {
  | (Some(a), Some(b)) => Some(a, b)
  | _ => None
  }

let map = (t, ~f) => Belt.Option.map(t, f)

let map2 = (a, b, ~f) =>
  switch (a, b) {
  | (Some(a), Some(b)) => Some(f(a, b))
  | _ => None
  }

let unwrap = (t, ~default) => Belt.Option.getWithDefault(t, default)

let unwrapOrFailWith = (t, ~exn) =>
  switch t {
  | Some(value) => value
  | None => raise(exn)
  }

let unwrapUnsafe = unwrapOrFailWith(~exn=Invalid_argument("Option.unwrapUnsafe called with None"))

let unwrap_unsafe = unwrapUnsafe

let toArray = t =>
  switch t {
  | None => []
  | Some(value) => [value]
  }

let to_array = toArray

let toList = t =>
  switch t {
  | None => list{}
  | Some(value) => list{value}
  }

let to_list = toList

let tap = (t, ~f) =>
  switch t {
  | None => ()
  | Some(x) => f(x)
  }

let equal = (equal, a, b) =>
  switch (a, b) {
  | (None, None) => true
  | (Some(a'), Some(b')) => equal(a', b')
  | _ => false
  }

let compare = (compare, a, b) =>
  switch (a, b) {
  | (None, None) => 0
  | (Some(a'), Some(b')) => compare(a', b')
  | (None, Some(_)) => -1
  | (Some(_), None) => 1
  }

let \"|?" = (t, default) => unwrap(t, ~default)

let \">>|" = (t, f) => map(t, ~f)

let \">>=" = (t, f) => andThen(t, ~f)
