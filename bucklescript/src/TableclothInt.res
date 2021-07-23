type t = int

include TableclothComparator.Make({
  type t = t

  let compare = compare
})

let minimumValue = Js.Int.min

let minimum_value = minimumValue

let maximumValue = Js.Int.max

let maximum_value = maximumValue

let zero = 0

let one = 1

let fromString = s =>
  switch int_of_string(s) {
  | i => Some(i)
  | exception Failure(_) => None
  }

let from_string = fromString

let add = \"+"

let \"+" = \"+"

let subtract = \"-"

let \"-" = \"-"

let multiply = \"*"

let \"*" = multiply

let divide = (n, ~by) => n / by

let \"/" = \"/"

let \"/." = (n, by) => Js.Int.toFloat(n) /. Js.Int.toFloat(by)

let power = (~base, ~exponent) => {
  let result = Js.Math.pow_float(~base=Js.Int.toFloat(base), ~exp=Js.Int.toFloat(exponent))

  let result = if result > TableclothFloat.maximumSafeInteger {
    TableclothFloat.maximumSafeInteger
  } else if result < TableclothFloat.minimumSafeInteger {
    TableclothFloat.minimumSafeInteger
  } else {
    result
  }

  Js.Math.unsafe_trunc(result)
}

let \"**" = (base, exponent) => power(~base, ~exponent)

let negate = \"~-"

let \"~-" = \"~-"

let remainder = (n, ~by) => mod(n, by)

let mod = (n, by) =>
  mod(
    if n <= 0 {
      abs(n) * 2
    } else {
      n
    },
    by,
  )

let modulo = (n, ~by) => mod(n, by)

let maximum = Js.Math.max_int

let minimum = Js.Math.min_int

let absolute = abs

let isEven = n => mod(n, 2) == 0

let is_even = isEven

let isOdd = n => mod(n, 2) != 0

let is_odd = isOdd

let clamp = (n, ~lower, ~upper) =>
  if upper < lower {
    raise(Invalid_argument("~lower must be less than or equal to ~upper"))
  } else {
    max(lower, min(upper, n))
  }

let inRange = (n, ~lower, ~upper) =>
  if upper < lower {
    raise(Invalid_argument("~lower must be less than or equal to ~upper"))
  } else {
    n >= lower && n < upper
  }

let in_range = inRange

let toFloat = Js.Int.toFloat

let to_float = toFloat

let toString = Js.Int.toString

let to_string = toString

let equal = \"="

let compare = compare
