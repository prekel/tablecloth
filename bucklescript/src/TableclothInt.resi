@@ocaml.text(" ")

@@ocaml.text(" The platform-dependant {{: https://en.wikipedia.org/wiki/Signed_number_representations } signed } {{: https://en.wikipedia.org/wiki/Integer } integer} type.

    An [int] is a whole number.

    [int]s are subject to {{: https://en.wikipedia.org/wiki/Integer_overflow } overflow }, meaning that [Int.maximumValue + 1 = Int.minimumValue].

    If you need to work with integers larger than {!maximumValue} (or smaller than {!minimumValue} you can use the {!Integer} module.

    Valid syntax for [int]s includes:
    {[
      0
      42
      9000
      1_000_000
      1_000_000
      0xFF (* 255 in hexadecimal *)
      0x000A (* 10 in hexadecimal *)
    ]}

    {b Note:} The number of bits used for an [int] is platform dependent.

    When targeting Bucklescript {{: Ints are 32 bits} https://bucklescript.github.io/docs/en/common-data-types.html#int }.

    When targeting native OCaml uses 31-bits on 32-bit platforms and 63-bits on 64-bit platforms
    which means that [int] math is well-defined in the range [-2 ** 30] to [2 ** 30 - 1] for 32bit platforms [-2 ** 62] to [2 ** 62 - 1] for 64bit platforms.

    Outside of that range, the behavior is determined by the compilation target.

    You can read about the reasons for OCaml's unusual integer sizes {{: https://v1.realworldocaml.org/v1/en/html/memory-representation-of-values.html} here }.

    {e Historical Note: } The name [int] comes from the term {{: https://en.wikipedia.org/wiki/Integer } integer}). It appears
    that the [int] abbreviation was introduced in the programming language ALGOL 68.

    Today, almost all programming languages use this abbreviation.
")

type t = int

@@ocaml.text(" {1 Constants } ")

@ocaml.doc(" The literal [0] as a named value ")
let zero: t

@ocaml.doc(" The literal [1] as a named value ")
let one: t

@ocaml.doc(" The maximum representable [int] on the current platform ")
let maximumValue: t

let maximum_value: t

@ocaml.doc(" The minimum representable [int] on the current platform ")
let minimumValue: t

let minimum_value: t

@@ocaml.text(" {1 Create} ")

@ocaml.doc(" Attempt to parse a [string] into a [int].

    {2 Examples}

    {[Int.fromString \"0\" = Some 0.]}

    {[Int.fromString \"42\" = Some 42.]}

    {[Int.fromString \"-3\" = Some (-3)]}

    {[Int.fromString \"123_456\" = Some 123_456]}

    {[Int.fromString \"0xFF\" = Some 255]}

    {[Int.fromString \"0x00A\" = Some 10]}

    {[Int.fromString \"Infinity\" = None]}

    {[Int.fromString \"NaN\" = None]}
")
let fromString: string => option<t>

let from_string: string => option<t>

@@ocaml.text(" {1 Operators}

    {b Note } You do not need to open the {!Int} module to use the
    {!( + )}, {!( - )}, {!( * )}, {!( ** )}, {! (mod)} or {!( / )} operators, these are
    available as soon as you [open Tablecloth]
")

@ocaml.doc(" Add two {!Int} numbers.

  {[Int.add 3002 4004 = 7006]}

  Or using the globally available operator:

  {[3002 + 4004 = 7006]}

  You {e cannot } add an [int] and a [float] directly though.

  See {!Float.add} for why, and how to overcome this limitation.
")
let add: (t, t) => t

@ocaml.doc(" See {!Int.add} ")
let \"+": (t, t) => t

@ocaml.doc(" Subtract numbers

    {[Int.subtract 4 3 = 1]}

    Alternatively the operator can be used:

    {[4 - 3 = 1]}
")
let subtract: (t, t) => t

@ocaml.doc(" See {!Int.subtract} ")
let \"-": (t, t) => t

@ocaml.doc(" Multiply [int]s like

    {[Int.multiply 2 7 = 14]}

    Alternatively the operator can be used:

    {[(2 * 7) = 14]}
")
let multiply: (t, t) => t

@ocaml.doc(" See {!Int.multiply} ")
let \"*": (t, t) => t

@ocaml.doc(" Integer division

    Notice that the remainder is discarded.

    {3 Exceptions}

    Throws [Division_by_zero] when the divisor is [0].

    {2 Examples}

    {[Int.divide 3 ~by:2 = 1]}

    {[27 / 5 = 5]}
")
let divide: (t, ~by: t) => t

@ocaml.doc(" See {!Int.divide} ")
let \"/": (t, t) => t

@ocaml.doc(" Floating point division

    {2 Examples}

    {[Int.(3 /. 2) = 1.5]}

    {[Int.(27 /. 5) = 5.25]}

    {[Int.(8 /. 4) = 2.0]}
")
let \"/.": (t, t) => float

@ocaml.doc(" Exponentiation, takes the base first, then the exponent.

    {2 Examples}

    {[Int.power ~base:7 ~exponent:3 = 343]}

    Alternatively the [**] operator can be used:

    {[7 ** 3 = 343]}
")
let power: (~base: t, ~exponent: t) => t

@ocaml.doc(" See {!Int.power} ")
let \"**": (t, t) => t

@ocaml.doc(" Flips the 'sign' of an integer so that positive integers become negative and negative integers become positive. Zero stays as it is.

    {2 Examples}

    {[Int.negate 8 = (-8)]}

    {[Int.negate (-7) = 7]}

    {[Int.negate 0 = 0]}

    Alternatively the [~-] operator can be used:

    {[~-(7) = (-7)]}
")
let negate: t => t

@ocaml.doc(" See {!Int.negate} ")
let \"~-": t => t

@ocaml.doc(" Get the {{: https://en.wikipedia.org/wiki/Absolute_value } absolute value } of a number.

    {2 Examples}

    {[Int.absolute 8 = 8]}

    {[Int.absolute (-7) = 7]}

    {[Int.absolute 0 = 0]}
")
let absolute: t => t

@ocaml.doc(" Perform {{: https://en.wikipedia.org/wiki/Modular_arithmetic } modular arithmetic }.

    If you intend to use [modulo] to detect even and odd numbers consider using {!Int.isEven} or {!Int.isOdd}.

    The [modulo] function works in the typical mathematical way when you run into negative numbers

    Use {!Int.remainder} for a different treatment of negative numbers.

    {2 Examples}

    {[Int.modulo ~by:3 (-4) = 1]}

    {[Int.modulo ~by:3 (-3 )= 0]}

    {[Int.modulo ~by:3 (-2) = 2]}

    {[Int.modulo ~by:3 (-1) = 1]}

    {[Int.modulo ~by:3 0 = 0]}

    {[Int.modulo ~by:3 1 = 1]}

    {[Int.modulo ~by:3 2 = 2]}

    {[Int.modulo ~by:3 3 = 0]}

    {[Int.modulo ~by:3 4 = 1]}
")
let modulo: (t, ~by: t) => t

@ocaml.doc(" See {!Int.modulo} ")
let mod: (t, t) => t

@ocaml.doc(" Get the remainder after division. Here are bunch of examples of dividing by four:

    Use {!Int.modulo} for a different treatment of negative numbers.

    {2 Examples}

    {[
      List.map
        ~f:(Int.remainder ~by:4)
        [(-5); (-4); (-3); (-2); (-1); 0; 1; 2; 3; 4; 5] =
          [(-1); 0; (-3); (-2); (-1); 0; 1; 2; 3; 0; 1]
    ]}
")
let remainder: (t, ~by: t) => t

@ocaml.doc(" Returns the larger of two [int]s

    {2 Examples}

    {[Int.maximum 7 9 = 9]}

    {[Int.maximum (-4) (-1) = (-1)]}
")
let maximum: (t, t) => t

@ocaml.doc(" Returns the smaller of two [int]s

    {2 Examples}

    {[Int.minimum 7 9 = 7]}

    {[Int.minimum (-4) (-1) = (-4)]}
")
let minimum: (t, t) => t

@@ocaml.text(" {1 Query} ")

@ocaml.doc(" Check if an [int] is even

    {2 Examples}

    {[Int.isEven 8 = true]}

    {[Int.isEven 7 = false]}

    {[Int.isEven 0 = true]}
")
let isEven: t => bool

let is_even: t => bool

@ocaml.doc(" Check if an [int] is odd

  {2 Examples}

  {[Int.isOdd 7 = true]}

  {[Int.isOdd 8 = false]}

  {[Int.isOdd 0 = false]}
")
let isOdd: t => bool

let is_odd: t => bool

@ocaml.doc(" Clamps [n] within the inclusive [lower] and [upper] bounds.

  {3 Exceptions}

  Throws an [Invalid_argument] exception if [lower > upper]

  {2 Examples}

  {[Int.clamp ~lower:0 ~upper:8 5 = 5]}

  {[Int.clamp ~lower:0 ~upper:8 9 = 8]}

  {[Int.clamp ~lower:(-10) ~upper:(-5) 5 = (-5)]}
")
let clamp: (t, ~lower: t, ~upper: t) => t

@ocaml.doc(" Checks if [n] is between [lower] and up to, but not including, [upper].

    {3 Exceptions}

    Throws an [Invalid_argument] exception if [lower > upper]

    {2 Examples}

    {[Int.inRange ~lower:2 ~upper:4 3 = true]}

    {[Int.inRange ~lower:5 ~upper:8 4 = false]}

    {[Int.inRange ~lower:(-6) ~upper:(-2) (-3) = true]}

")
let inRange: (t, ~lower: t, ~upper: t) => bool

let in_range: (t, ~lower: t, ~upper: t) => bool

@@ocaml.text(" {1 Convert} ")

@ocaml.doc(" Convert an integer into a float. Useful when mixing {!Int} and {!Float} values like this:

    {2 Examples}

    {[
      let halfOf (number : int) : float =
        Float.((Int.toFloat number) / 2)
        (* Note that locally opening the {!Float} module here allows us to use the floating point division operator *)
      in
      halfOf 7 = 3.5
    ]}
")
let toFloat: t => float

let to_float: t => float

@ocaml.doc(" Convert an [int] into a [string] representation.

    Guarantees that

    {[Int.(fromString (toString n)) = Some n ]}

    {2 Examples}

    {[Int.toString 3 = \"3\"]}

    {[Int.toString (-3) = \"-3\"]}

    {[Int.to_sString 0 = \"0\"]}
")
let toString: t => string

let to_string: t => string

@@ocaml.text(" {1 Compare} ")

@ocaml.doc(" Test two [int]s for equality ")
let equal: (t, t) => bool

@ocaml.doc(" Compare two [int]s ")
let compare: (t, t) => int

@ocaml.doc(" The unique identity for {!Comparator} ")
type identity

let comparator: TableclothComparator.t<t, identity>
