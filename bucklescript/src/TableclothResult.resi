@@ocaml.text(" ")

@@ocaml.text(" A {!Result} is used to represent a computation which may fail.

    A [Result] is a variant, which has a constructor for successful results
    [(Ok 'ok)], and one for unsuccessful results ([(Error 'error)]).

    {[
      type ('ok, 'error) t =
        | Ok of 'ok
        | Error of 'error
    ]}

    Here is how you would annotate a [Result] variable whose [Ok]
    variant is an integer and whose [Error] variant is a string:

    {[let ok: (int, string) Result.t = Ok 3]}

    {[let error: (int, string) Result.t = Error \"This computation failed!\"]}

    {b Note} The ['error] case can be of {b any} type and while [string] is very common you could also use:
    - [string List.t] to allow errors to be accumulated
    - [exn], in which case the result type just makes exceptions explicit in the return type
    - A variant or polymorphic variant, with one case per possible error. This is means each error can be dealt with explicitly. See {{: https://keleshev.com/composable-error-handling-in-ocaml } this excellent article} for mnore information on this approach.

    If the function you are writing can only fail in a single obvious way, maybe you want an {!Option} instead.
")

type t<'ok, 'error> = result<'ok, 'error>

@@ocaml.text(" {1 Create} ")

@ocaml.doc(" A function alternative to the [Ok] constructor which can be used in places where
    the constructor isn't permitted such as at the of a {!Fun.(|>)} or functions like {!List.map}.

    {2 Examples}

    {[String.reverse \"desserts\" |> Result.ok = Ok \"stressed\"]}

    {[List.map [1; 2; 3] ~f:Result.ok = [Ok 1; Ok 2; Ok 3]]}
")
let ok: 'ok => t<'ok, 'error>

@ocaml.doc(" A function alternative to the [Error] constructor which can be used in places where
    the constructor isn't permitted such as at the of a {!Fun.pipe} or functions like {!List.map}.

    {b Note}

    When targetting the Bucklescript compiler you {b can} use constructors with the fast pipe.

    {[5 |. Ok = (Ok 5)]}

    See the {{: https://reasonml.github.io/docs/en/pipe-first#pipe-into-variants} Reason docs } for more.

    {2 Examples}

    {[Int.negate 3 |> Result.error 3 = Error (-3)]}

    {[List.map [1; 2; 3] ~f:Result.error = [Error 1; Error 2; Error 3]]}
")
let error: 'error => t<'ok, 'error>

@ocaml.doc(" Run the provided function and wrap the returned value in a {!Result}, catching any exceptions raised.

    {2 Examples}

    {[Result.attempt (fun () -> 5 / 0) = Error Division_by_zero]}

    {[
      let numbers = [|1,2,3|] in
      Result.attempt (fun () -> numbers.(3)) =
        Error (Invalid_argument \"index out of bounds\")
    ]}
")
let attempt: (unit => 'ok) => t<'ok, exn>

@ocaml.doc(" Convert an {!Option} to a {!Result} where a [(Some value)] becomes [(Ok value)] and a [None] becomes [(Error error)].

    {2 Examples}

    {[Result.fromOption (Some 84) ~error:\"Greater than 100\" = Ok 8]}

    {[
      Result.fromOption None ~error:\"Greater than 100\" =
        Error \"Greater than 100\"
    ]}
")
let fromOption: (option<'ok>, ~error: 'error) => t<'ok, 'error>

let from_option: (option<'ok>, ~error: 'error) => t<'ok, 'error>

@ocaml.doc(" Check if a {!Result} is an [Ok].

    Useful when you want to perform some side affect based on the presence of
    an [Ok] like logging.

    {b Note} if you need access to the contained value rather than doing
    [Result.isOk] followed by {!Result.unwrapUnsafe} its safer and just as
    convenient to use pattern matching directly or use one of {!Result.andThen}
    or {!Result.map}

    {2 Examples}

    {[Result.isOk (Ok 3) = true]}

    {[Result.isOk (Error 3) = false]}
")
let isOk: t<_, _> => bool

let is_ok: t<_, _> => bool

@ocaml.doc(" Check if a {!Result} is an [Error].

    Useful when you want to perform some side affect based on the presence of
    an [Error] like logging.

    {b Note} if you need access to the contained value rather than doing
    {!Result.isOk} followed by {!Result.unwrapUnsafe} its safer and just as
    convenient to use pattern matching directly or use one of {!Result.andThen}
    or {!Result.map}

    {2 Examples}

    {[Result.isError (Ok 3) = false]}

    {[Result.isError (Error 3) = true]}
")
let isError: t<_, _> => bool

let is_error: t<_, _> => bool

@ocaml.doc(" Returns the first argument if it {!isError}, otherwise return the second argument.

    Unlike the {!Bool.(&&)} operator, the [and_] function does not short-circuit.
    When you call [and_], both arguments are evaluated before being passed to the function.

    {2 Examples}

    {[Result.and_ (Ok \"Antelope\") (Ok \"Salmon\") = Ok \"Salmon\"]}

    {[
      Result.and_
        (Error (`UnexpectedBird \"Finch\"))
        (Ok \"Salmon\")
        = Error (`UnexpectedBird \"Finch\")
    ]}

    {[
      Result.and_
        (Ok \"Antelope\")
        (Error (`UnexpectedBird \"Finch\"))
          = Error (`UnexpectedBird \"Finch\")
    ]}

    {[
      Result.and_
        (Error (`UnexpectedInvertabrate \"Honey bee\"))
        (Error (`UnexpectedBird \"Finch\"))
          = Error (`UnexpectedBird \"Honey Bee\")
    ]}
")
let and_: (t<'ok, 'error>, t<'ok, 'error>) => t<'ok, 'error>

@ocaml.doc(" Return the first argument if it {!isOk}, otherwise return the second.

  Unlike the built in [||] operator, the [or_] function does not short-circuit.
  When you call [or_], both arguments are evaluated before being passed to the function.

  {2 Examples}

  {[Result.or_ (Ok \"Boar\") (Ok \"Gecko\") = (Ok \"Boar\")]}

  {[Result.or_ (Error (`UnexpectedInvertabrate \"Periwinkle\")) (Ok \"Gecko\") = (Ok \"Gecko\")]}

  {[Result.or_ (Ok \"Boar\") (Error (`UnexpectedInvertabrate \"Periwinkle\")) = (Ok \"Boar\") ]}

  {[Result.or_ (Error (`UnexpectedInvertabrate \"Periwinkle\")) (Error (`UnexpectedBird \"Robin\")) = (Error (`UnexpectedBird \"Robin\"))]}
")
let or_: (t<'ok, 'error>, t<'ok, 'error>) => t<'ok, 'error>

@ocaml.doc(" Combine two results, if both are [Ok] returns an [Ok] containing a {!Tuple} of the values.

    If either is an [Error], returns the [Error].

    The same as writing [Result.map2 ~f:Tuple.make]

    {2 Examples}

    {[Result.both (Ok \"Badger\") (Ok \"Rhino\") = Ok (\"Dog\", \"Rhino\")]}

    {[
      Result.both (Error (`UnexpectedBird \"Flamingo\")) (Ok \"Rhino\") =
        (Error (`UnexpectedBird \"Flamingo\"))
    ]}

    {[
      Result.both
        (Ok \"Badger\")
        (Error (`UnexpectedInvertabrate \"Blue ringed octopus\")) =
          (Error (`UnexpectedInvertabrate \"Blue ringed octopus\"))
    ]}

    {[
      Result.both
        (Error (`UnexpectedBird \"Flamingo\"))
        (Error (`UnexpectedInvertabrate \"Blue ringed octopus\")) =
          (Error (`UnexpectedBird \"Flamingo\"))
    ]}
")
let both: (t<'a, 'error>, t<'b, 'error>) => t<('a, 'b), 'error>

@ocaml.doc(" Collapse a nested result, removing one layer of nesting.

    {2 Examples}

    {[Result.flatten (Ok (Ok 2)) = Ok 2]}

    {[
      Result.flatten (Ok (Error (`UnexpectedBird \"Peregrin falcon\"))) =
        (Error (`UnexpectedBird \"Peregrin falcon\"))
    ]}

    {[
      Result.flatten (Error (`UnexpectedInvertabrate \"Woodlouse\")) =
        (Error (`UnexpectedInvertabrate \"Woodlouse\"))
    ]}
")
let flatten: t<t<'ok, 'error>, 'error> => t<'ok, 'error>

@ocaml.doc(" Unwrap a Result using the [~default] value in case of an [Error]

    {2 Examples}

    {[Result.unwrap ~default:0 (Ok 12) = 12]}

    {[Result.unwrap ~default:0 ((Error (`UnexpectedBird \"Ostrich\"))) = 0]}
")
let unwrap: (t<'ok, 'error>, ~default: 'ok) => 'ok

@ocaml.doc(" Unwrap a Result, raising an exception in case of an [Error]

    {e Exceptions}

    Raises an [Invalid_argument \"Result.unwrapUnsafe called with an Error\"] exception.

    {2 Examples}

    {[Result.unwrapUnsafe (Ok 12) = 12]}

    {[Result.unwrapUnsafe (Error \"bad\") ]}
")
let unwrapUnsafe: t<'ok, _> => 'ok

let unwrap_unsafe: t<'ok, _> => 'ok

@ocaml.doc(" Like {!Result.unwrap} but unwraps an [Error] value instead

    {2 Examples}

    {[
      Result.unwrapError
        (Error (`UnexpectedBird \"Swallow\"))
        ~default:(`UnexpectedInvertabrate \"Ladybird\") =
          `UnexpectedBird \"Swallow\"
    ]}

    {[
      Result.unwrapError
        (Ok 5)
        ~default:(`UnexpectedInvertabrate \"Ladybird\") =
          `UnexpectedInvertabrate \"Ladybird\"
    ]}
")
let unwrapError: (t<'ok, 'error>, ~default: 'error) => 'error

let unwrap_error: (t<'ok, 'error>, ~default: 'error) => 'error

@ocaml.doc(" Combine two results

    If one of the results is an [Error], that becomes the return result.

    If both are [Error] values, returns its first.

    {2 Examples}

    {[Result.map2 (Ok 7) (Ok 3) ~f:Int.add = Ok 10]}

    {[Result.map2 (Error \"A\") (Ok 3) ~f:Int.add = Error \"A\"]}

    {[Result.map2 (Ok 7) (Error \"B\") ~f:Int.add = Error \"B\"]}

    {[Result.map2 (Error \"A\") (Error \"B\") ~f:Int.add = Error \"A\"]}
")
let map2: (t<'a, 'error>, t<'b, 'error>, ~f: ('a, 'b) => 'c) => t<'c, 'error>

@ocaml.doc(" If all of the elements of a list are [Ok], returns an [Ok] of the the list of unwrapped values.

    If {b any} of the elements are an [Error], the first one encountered is returned.

    {2 Examples}

    {[Result.values [Ok 1; Ok 2; Ok 3; Ok 4] = Ok [1; 2; 3; 4]]}

    {[Result.values [Ok 1; Error \"two\"; Ok 3; Error \"four\"] = Error \"two\"]}
")
let values: list<t<'ok, 'error>> => t<list<'ok>, 'error>

@ocaml.doc(" Transforms the ['ok] in a result using [f]. Leaves the ['error] untouched.

    {2 Examples}

    {[Result.map (Ok 3) ~f:(Int.add 1) = Ok 9]}

    {[Result.map (Error \"three\") ~f:(Int.add 1) = Error \"three\"]}
")
let map: (t<'a, 'error>, ~f: 'a => 'b) => t<'b, 'error>

@ocaml.doc(" Transforms the value in an [Error] using [f]. Leaves an [Ok] untouched.

    {2 Examples}

    {[Result.mapError (Ok 3) ~f:String.reverse = Ok 3]}

    {[Result.mapError (Error \"bad\") ~f:(Int.add 1)  = Error \"bad\"]}
")
let mapError: (t<'ok, 'a>, ~f: 'a => 'b) => t<'ok, 'b>

let map_error: (t<'ok, 'a>, ~f: 'a => 'b) => t<'ok, 'b>

@ocaml.doc(" Run a function which may fail on a result.

    Short-circuits of called with an [Error].

    {2 Examples}

    {[
      let reciprical (x:float) : (string, float) Standard.Result.t = (
        if (x = 0.0) then
          Error \"Divide by zero\"
        else
          Ok (1.0 /. x)
      )

      let root (x:float) : (string, float) Standard.Result.t = (
        if (x < 0.0) then
          Error \"Cannot be negative\"
        else
          Ok (Float.squareRoot x)
      )
    ]}

    {2 Examples}

    {[Result.andThen ~f:reciprical (Ok 4.0) = Ok 0.25]}

    {[Result.andThen ~f:reciprical (Error \"Missing number!\") = Error \"Missing number!\"]}

    {[Result.andThen ~f:reciprical (Ok 0.0) = Error \"Divide by zero\"]}

    {[Result.andThen (Ok 4.0) ~f:root  |> Result.andThen ~f:reciprical = Ok 0.5]}

    {[Result.andThen (Ok -2.0) ~f:root |> Result.andThen ~f:reciprical = Error \"Cannot be negative\"]}

    {[Result.andThen (Ok 0.0) ~f:root |> Result.andThen ~f:reciprical = Error \"Divide by zero\"]}
")
let andThen: (t<'a, 'error>, ~f: 'a => t<'b, 'error>) => t<'b, 'error>

let and_then: (t<'a, 'error>, ~f: 'a => t<'b, 'error>) => t<'b, 'error>

@ocaml.doc(" Run a function against an [(Ok value)], ignores [Error]s.

    {2 Examples}

    {[
      Result.tap (Ok \"Dog\") ~f:print_endline
      (* prints \"Dog\" *)
    ]}
 ")
let tap: (t<'ok, _>, ~f: 'ok => unit) => unit

@@ocaml.text(" {1 Convert} ")

@ocaml.doc(" Convert a {!Result} to an {!Option}.

    An [Ok x] becomes [Some x]

    An [Error _] becomes [None]

    {2 Examples}

    {[Result.toOption (Ok 42) = Some 42]}

    {[Result.toOption (Error \"Missing number!\") = None]}
")
let toOption: t<'ok, _> => option<'ok>

let to_option: t<'ok, _> => option<'ok>

@@ocaml.text(" {1 Compare} ")

@ocaml.doc(" Test two results for equality using the provided functions.

    {2 Examples}

    {[Result.equal String.equal Int.equal (Ok 3) (Ok 3) = true]}

    {[Result.equal String.equal Int.equal (Ok 3) (Ok 4) = false]}

    {[Result.equal String.equal Int.equal (Error \"Fail\") (Error \"Fail\") = true]}

    {[Result.equal String.equal Int.equal (Error \"Expected error\") (Error \"Unexpected error\") = false]}

    {[Result.equal String.equal Int.equal (Error \"Fail\") (Ok 4) = false]}
")
let equal: (('ok, 'ok) => bool, ('error, 'error) => bool, t<'ok, 'error>, t<'ok, 'error>) => bool

@ocaml.doc(" Compare results for using the provided functions.

    In the case when one of the results is an [Error] and one is [Ok], [Error]s  are considered 'less' then [Ok]s

    {2 Examples}

    {[Result.compare String.compare Int.compare (Ok 3) (Ok 3) = 0]}

    {[Result.compare String.compare Int.compare (Ok 3) (Ok 4) = -1]}

    {[Result.compare String.compare Int.compare (Error \"Fail\") (Error \"Fail\") = 0]}

    {[Result.compare String.compare Int.compare (Error \"Fail\") (Ok 4) = -1]}

    {[Result.compare String.compare Int.compare (Ok 4) (Error \"Fail\") = 1]}

    {[Result.compare String.compare Int.compare (Error \"Expected error\") (Error \"Unexpected error\") = -1]}
")
let compare: (('ok, 'ok) => int, ('error, 'error) => int, t<'ok, 'error>, t<'ok, 'error>) => int

@@ocaml.text(" {1 Operators}

    In functions that make heavy use of {!Result}s operators can make code significantly more
    concise at the expense  of placing a greater cognitive burden on future readers.
")

@ocaml.doc(" An operator version of {!Result.unwrap} where the [default] value goes to the right of the operator.

    {2 Examples}

    The following eamples assume [open Result.Infix] is in scope.

    {[Ok 4 |? 8 = 4]}

    {[Error \"Missing number!\" |? 8 = 8]}
")
let \"|?": (t<'a, 'error>, 'a) => 'a

@ocaml.doc(" An operator version of {!andThen}

    {2 Examples}

    The following examples assume

    {[
      open Result.Infix

      let reciprical (x:float) : (string, float) Standard.Result.t =
        if (x = 0.0) then
          Error \"Divide by zero\"
        else
          Ok (1.0 /. x)
    ]}

    Is in scope.

    {[Ok 4. >>= reciprical = Ok 0.25]}

    {[Error \"Missing number!\" >>= reciprical = Error \"Missing number!\"]}

    {[Ok 0. >>= reciprical = Error \"Divide by zero\"]}
")
let \">>=": (t<'ok, 'error>, 'ok => t<'b, 'error>) => t<'b, 'error>

@ocaml.doc(" An operator version of {!map}

    {2 Examples}

    The following examples assume [open Result.Infix] is in scope.

    {[Ok 4 >>| Int.add(1) = Ok 5]}

    {[Error \"Its gone bad\" >>| Int.add(1) = Error \"Its gone bad\"]}
")
let \">>|": (t<'a, 'error>, 'a => 'b) => t<'b, 'error>

@ocaml.doc(" [Result.pp errFormat okFormat destFormat result] \"pretty-prints\"
    the [result], using [errFormat] if the [result] is an [Error] value or
    [okFormat] if the [result] is an [Ok] value. [destFormat] is a formatter
    that tells where to send the output.

    {[
      let good: (int, string) Result.t = Ok 42 in
      let not_good: (int, string) Tablecloth.Result.t = Error \"bad\" in
      Result.pp Format.pp_print_int Format.pp_print_string Format.std_formatter good;
      Result.pp Format.pp_print_int Format.pp_print_string Format.std_formatter not_good;
      Format.pp_print_newline Format.std_formatter ();
      (* prints <ok: 42><error: bad>*)
    ]}
  ")
let pp: (
  (Format.formatter, 'ok) => unit,
  (Format.formatter, 'error) => unit,
  Format.formatter,
  t<'ok, 'error>,
) => unit
