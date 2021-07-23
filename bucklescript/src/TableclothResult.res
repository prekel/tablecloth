type t<'ok, 'error> = result<'ok, 'error>

let ok = a => Belt.Result.Ok(a)

let error = e => Belt.Result.Error(e)

let fromOption = (ma, ~error) =>
  switch ma {
  | None => Belt.Result.Error(error)
  | Some(right) => Belt.Result.Ok(right)
  }

let from_option = fromOption

let isError = Belt.Result.isError

let is_error = isError

let isOk = Belt.Result.isOk

let is_ok = isOk

let both = (a, b) =>
  switch (a, b) {
  | (Ok(a'), Ok(b')) => Ok(a', b')
  | (Error(a'), _) => Error(a')
  | (_, Error(b')) => Error(b')
  }

let flatten = a =>
  switch a {
  | Ok(a') => a'
  | Error(error) => Error(error)
  }

let or_ = (a, b) =>
  switch a {
  | Ok(_) => a
  | _ => b
  }

let and_ = (a, b) =>
  switch a {
  | Ok(_) => b
  | _ => a
  }

let unwrap = (t, ~default) => Belt.Result.getWithDefault(t, default)

let unwrapUnsafe = t => Belt.Result.getExn(t)

let unwrap_unsafe = unwrapUnsafe

let unwrapError = (t, ~default) =>
  switch t {
  | Ok(_) => default
  | Error(value) => value
  }

let unwrap_error = unwrapError

let map2 = (a, b, ~f) =>
  switch (a, b) {
  | (Ok(a), Ok(b)) => Ok(f(a, b))
  | (Error(a), _) => Error(a)
  | (_, Error(b)) => Error(b)
  }

let values = t => List.fold_right(map2(~f=(a, b) => list{a, ...b}), t, Ok(list{}))

let map = (t, ~f) => Belt.Result.map(t, f)

let mapError = (t, ~f) =>
  switch t {
  | Error(error) => Error(f(error))
  | Ok(value) => Ok(value)
  }

let map_error = mapError

let toOption = r =>
  switch r {
  | Ok(v) => Some(v)
  | Error(_) => None
  }

let to_option = toOption

let andThen = (t, ~f) => Belt.Result.flatMap(t, f)

let and_then = andThen

let attempt = f =>
  switch f() {
  | value => Ok(value)
  | exception error => Error(error)
  }

let tap = (t, ~f) =>
  switch t {
  | Ok(a) => f(a)
  | _ => ()
  }

let equal = (equalOk, equalError, a, b) =>
  switch (a, b) {
  | (Error(a'), Error(b')) => equalError(a', b')
  | (Ok(a'), Ok(b')) => equalOk(a', b')
  | _ => false
  }

let compare = (
  compareOk: ('ok, 'ok) => int,
  compareError: ('error, 'error) => int,
  a: t<'ok, 'error>,
  b: t<'ok, 'error>,
): int =>
  switch (a, b) {
  | (Error(a'), Error(b')) => compareError(a', b')
  | (Ok(a'), Ok(b')) => compareOk(a', b')
  | (Error(_), Ok(_)) => -1
  | (Ok(_), Error(_)) => 1
  }

let \"|?" = (t, default) => unwrap(t, ~default)

let \">>|" = (t, f) => map(t, ~f)

let \">>=" = (t, f) => andThen(t, ~f)

let pp = (
  okf: (Format.formatter, 'ok) => unit,
  errf: (Format.formatter, 'error) => unit,
  fmt: Format.formatter,
  r: t<'ok, 'error>,
) =>
  switch r {
  | Ok(ok) =>
    Format.pp_print_string(fmt, "<ok: ")
    okf(fmt, ok)
    Format.pp_print_string(fmt, ">")
  | Error(err) =>
    Format.pp_print_string(fmt, "<error: ")
    errf(fmt, err)
    Format.pp_print_string(fmt, ">")
  }
