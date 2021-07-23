@@ocaml.text(" ")

@@ocaml.text(" Functions for working with functions.

    While the functions in this module can often make code more concise, this
    often imposes a readability burden on future readers.
")

@ocaml.doc(" Given a value, returns exactly the same value. This may seem pointless at first glance but it can often be useful when an api offers you more control than you actually need.

    Perhaps you want to create an array of integers

    {[Array.initialize 6 ~f:Fun.identity = [|0; 1; 2; 3; 4; 5|]]}

    (In this particular case you probably want to use {!Array.range}.)

    Or maybe you need to register a callback, but dont want to do anything:

    {[
      let httpMiddleware =
        HttpLibrary.createMiddleWare
          ~onEventYouDoCareAbout:transformAndReturn
          ~onEventYouDontCareAbout:Fun.identity
    ]}
")
external identity: 'a => 'a = "%identity"

@ocaml.doc(" Discards the value it is given and returns [()]

    This is primarily useful when working with imperative side-effecting code
    or to avoid [unused value] compiler warnings when you really meant it,
    and haven't just made a mistake.

    {2 Examples}

    {[
      (* Pretend we have a module with the following signature:
          module PretendMutableQueue : sig
            type 'a t

            (** Adds an element to the queue, returning the new length of the queue *)
            val pushReturningLength : 'a t -> 'a -> int
          end
      *)

      let addListToQueue queue list =
        List.forEach list ~f:(fun element ->
          ignore (MutableQueue.pushReturningLength queue element)
        )
      in ()
    ]}
")
external ignore: _ => unit = "%ignore"

@ocaml.doc(" Create a function that {b always} returns the same value.

    Useful with functions like {!List.map} or {!Array.initialize}

    {2 Examples}

    {[List.map ~f:(Fun.constant 0) [1;2;3;4;5] = [0;0;0;0;0]]}

    {[Array.initialize 6 ~f:(Fun.constant 0) = [|0;0;0;0;0;0|]]}
")
let constant: ('a, 'b) => 'a

@ocaml.doc(" A function which always returns its second argument. ")
let sequence: ('a, 'b) => 'b

@ocaml.doc(" Reverses the argument order of a function.

    For any arguments [x] and [y], [(flip f) x y] is the same as [f y x].

    Perhaps you want to [fold] something, but the arguments of a function you
    already have access to are in the wrong order.
")
let flip: (('a, 'b) => 'c, 'b, 'a) => 'c

@ocaml.doc(" Negate a function.

    This can be useful in combination with {!List.filter} / {!Array.filter} or {!List.find} / {!Array.find}

    {2 Examples}

    {[
      let isLessThanTwelve = Fun.negate (fun n -> n >= 12) in
      isLessThanTwelve 12 = false
    ]}
")
let negate: ('a => bool, 'a) => bool

@ocaml.doc(" See {!Fun.(<|)} ")
let apply: ('a => 'b, 'a) => 'b

@ocaml.doc(" Like {!(|>)} but in the opposite direction.

    [f <| x] is exactly the same as [f x].

    Maybe you want to apply a function to a [match] expression? That sort of thing.
")
let \"<|": ('a => 'b, 'a) => 'b

@ocaml.doc(" See {!Fun.(|>)} ")
external pipe: ('a, 'a => 'b) => 'b = "%revapply"

@ocaml.doc(" Saying [x |> f] is exactly the same as [f x], just a bit longer.

    It is called the \"pipe\" operator because it lets you write \"pipelined\" code.

    It can make nested function calls more readable.

    For example, say we have a [sanitize] function for turning user input into
    integers:

    {[
      (* Before *)
      let sanitize (input: string) : int option =
        Int.fromString (String.trim input)
    ]}

    We can rewrite it like this:

    {[
      (* After *)
      let sanitize (input: string) : int option =
        input
        |> String.trim
        |> Int.fromString
    ]}

    This can be overused! When you have three or four steps, the code often gets clearer if you break things out into
    some smaller piplines assigned to variables. Now the transformation has a name, maybe it could have a type annotation.

    It can often be more self-documenting that way!
")
external \"|>": ('a, 'a => 'b) => 'b = "%revapply"

@ocaml.doc(" Function composition, passing results along in the suggested direction.

    For example, the following code (in a very roundabout way) checks if a number divided by two is odd:

    {[let isHalfOdd = Fun.(not << Int.isEven << Int.divide ~by:2)]}

    You can think of this operator as equivalent to the following:

    {[(g << f) = (fun x -> g (f x))]}

    So our example expands out to something like this:

    {[let isHalfOdd = fun n -> not (Int.isEven (Int.divide ~by:2 n))]}
")
let compose: ('b => 'c, 'a => 'b, 'a) => 'c

@ocaml.doc(" See {!Fun.compose} ")
let \"<<": ('b => 'c, 'a => 'b, 'a) => 'c

@ocaml.doc(" Function composition, passing results along in the suggested direction.

    For example, the following code checks if the square root of a number is odd:

    {[Int.squareRoot >> Int.isEven >> not]}
")
let composeRight: ('a => 'b, 'b => 'c, 'a) => 'c

@ocaml.doc(" See {!Fun.composeRight} ")
let \">>": ('a => 'b, 'b => 'c, 'a) => 'c

@ocaml.doc(" Useful for performing some side affect in {!Fun.pipe}-lined code.

    Most commonly used to log a value in the middle of a pipeline of function calls.

    {2 Examples}

    {[
      let sanitize (input: string) : int option =
        input
        |> String.trim
        |> Fun.tap ~f:(fun trimmedString -> print_endline trimmedString)
        |> Int.fromString
    ]}

    {[
      Array.filter [|1;3;2;5;4;|] ~f:Int.isEven
      |> Fun.tap ~f:(fun numbers -> numbers.(0) <- 0)
      |> Fun.tap ~f:Array.reverseInPlace
      = [|4;0|]
    ]}
")
let tap: ('a, ~f: 'a => unit) => 'a

@ocaml.doc(" Runs the provided function, forever.

    If an exception is thrown, returns the exception
")
let forever: (unit => unit) => exn

@ocaml.doc(" Runs a function repeatedly.

    {2 Examples}

    {[
      let count = ref 0
      times(10, fun () -> (count <- !count + 1))
      !count = 10
    ]}
")
let times: (int, ~f: unit => unit) => unit

@ocaml.doc(" Takes a function [f] which takes a single argument of a tuple ['a * 'b] and returns a function which takes two arguments that can be partially applied.

    {2 Examples}

    {[
      let squareArea (width, height) = width * height in
      let curriedArea : float -> float -> float = curry squareArea in
      let sizes = [3, 4, 5] in
      List.map sizes ~f:(curriedArea 4) = [12; 16; 20]
    ]}
")
let curry: ((('a, 'b)) => 'c, 'a, 'b) => 'c

@ocaml.doc(" Takes a function which takes two arguments and returns a function which takes a single argument of a tuple.

    {2 Examples}

    {[
      let sum (a : int) (b: int) : int = a + b in
      let uncurriedSum : (int * int) -> int = uncurry add in
      uncurriedSum (3, 4) = 7
    ]}
")
let uncurry: (('a, 'b) => 'c, ('a, 'b)) => 'c

@ocaml.doc(" Like {!curry} but for a {!Tuple3} ")
let curry3: ((('a, 'b, 'c)) => 'd, 'a, 'b, 'c) => 'd

@ocaml.doc(" Like {!uncurry} but for a {!Tuple3} ")
let uncurry3: (('a, 'b, 'c) => 'd, ('a, 'b, 'c)) => 'd
