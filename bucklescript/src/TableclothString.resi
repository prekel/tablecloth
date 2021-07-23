@@ocaml.text(" ")

@@ocaml.text(" Functions for working with [\"strings\"] ")

type t = string

@@ocaml.text(" {1 Create}

    Strings literals are created with the [\"double quotes\"] syntax.
")

@ocaml.doc(" Converts the given character to an equivalent string of length one. ")
let fromChar: char => string

let from_char: char => string

@ocaml.doc(" Create a string from an {!Array} of characters.

    Note that these must be individual characters in single quotes, not strings of length one.

    {2 Examples}

    {[String.fromArray [||] = \"\"]}

    {[String.fromArray [|'a'; 'b'; 'c'|] = \"abc\"]}
")
let fromArray: array<char> => string

let from_array: array<char> => string

@ocaml.doc(" Create a string from a {!List} of characters.

    Note that these must be individual characters in single quotes, not strings of length one.

    {2 Examples}

    {[String.fromList [] = \"\"]}

    {[String.fromList ['a'; 'b'; 'c'] = \"abc\"]}
")
let fromList: list<char> => string

let from_list: list<char> => string

@ocaml.doc(" Create a string by repeating a string [count] time.

    {3 Exceptions}

    If [count] is negative, [String.repeat] throws a [RangeError] exception.

    {2 Examples}

    {[String.repeat ~count:3 \"ok\" = \"okokok\"]}

    {[String.repeat ~count:3 \"\" = \"\"]}

    {[String.repeat ~count:0 \"ok\" = \"\"]}
")
let repeat: (string, ~count: int) => string

@ocaml.doc(" Create a string by providing a length and a function to choose characters.

    Returns an empty string if the length is negative.

    {2 Examples}

    {[String.initialize 8 ~f:(Fun.constant '9') = \"999999999\"]}
")
let initialize: (int, ~f: int => char) => string

@@ocaml.text(" {1 Basic operations} ")

@ocaml.doc(" Get the character at the specified index ")
let get: (string, int) => char

@ocaml.doc(" Get the character at [~index] ")
let getAt: (string, ~index: int) => option<char>

let get_at: (string, ~index: int) => option<char>

@ocaml.doc(" The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!getAt}

    {b Note} Currently this is only supported by the OCaml syntax.

    {2 Examples}

    {[(\"Doggie\".String.?[3]) = Some 'g']}

    {[String.(\"Doggie\".?[9]) = None]}
 ")
let \".?[]": (string, int) => option<char>

@ocaml.doc(" Reverse a string

    {b Note} This function does not work with Unicode characters.

    {2 Examples}

    {[String.reverse \"stressed\" = \"desserts\"]}
")
let reverse: string => string

@ocaml.doc(" Extract a substring from the specified indicies.

    See {!Array.slice}.
")
let slice: (~to_: int=?, string, ~from: int) => string

@@ocaml.text(" {1 Query} ")

@ocaml.doc(" Check if a string is empty ")
let isEmpty: string => bool

let is_empty: string => bool

@ocaml.doc(" Returns the length of the given string.

    {b Warning} if the string contains non-ASCII characters then [length] will
    not equal the number of characters

    {2 Examples}

    {[String.length \"abc\" = 3]}
")
let length: string => int

@ocaml.doc(" See if the second string starts with [prefix]

    {2 Examples}

    {[String.startsWith ~prefix:\"the\" \"theory\" = true]}

    {[String.startsWith ~prefix:\"ory\" \"theory\" = false]}
")
let startsWith: (string, ~prefix: string) => bool

let starts_with: (string, ~prefix: string) => bool

@ocaml.doc(" See if the second string ends with [suffix].

    {2 Examples}

    {[String.endsWith ~suffix:\"the\" \"theory\" = false]}

    {[String.endsWith ~suffix:\"ory\" \"theory\" = true]}
")
let endsWith: (string, ~suffix: string) => bool

let ends_with: (string, ~suffix: string) => bool

@ocaml.doc(" Check if one string appears within another

    {2 Examples}

    {[String.includes \"team\" ~substring:\"tea\" = true]}

    {[String.includes \"team\" ~substring:\"i\" = false]}

    {[String.includes \"ABC\" ~substring:\"\" = true]}
")
let includes: (string, ~substring: string) => bool

@ocaml.doc(" Test if the first letter of a string is upper case.

    {b Note} This function works only with ASCII characters, not Unicode.

    {2 Examples}

    {[String.isCapitalized \"Anastasia\" = true]}

    {[String.isCapitalized \"\" = false]}
")
let isCapitalized: string => bool

let is_capitalized: string => bool

@@ocaml.text(" {1 Modify} ")

@ocaml.doc(" Drop [count] characters from the left side of a string.

    {2 Examples}

    {[String.dropLeft ~count:3 \"abcdefg\" = \"defg\"]}

    {[String.dropLeft ~count:0 \"abcdefg\" = \"abcdefg\"]}

    {[String.dropLeft ~count:7 \"abcdefg\" = \"\"]}

    {[String.dropLeft ~count:(-2) \"abcdefg\" = \"fg\"]}

    {[String.dropLeft ~count:8 \"abcdefg\" = \"\"]}
")
let dropLeft: (string, ~count: int) => string

let drop_left: (string, ~count: int) => string

@ocaml.doc(" Drop [count] characters from the right side of a string.

    {2 Examples}

    {[String.dropRight ~count:3 \"abcdefg\" = \"abcd\"]}

    {[String.dropRight ~count:0 \"abcdefg\" = \"abcdefg\"]}

    {[String.dropRight ~count:7 \"abcdefg\" = \"\"]}

    {[String.dropRight ~count:(-2) \"abcdefg\" = \"abcdefg\"]}

    {[String.dropRight ~count:8 \"abcdefg\" = \"\"]}
")
let dropRight: (string, ~count: int) => string

let drop_right: (string, ~count: int) => string

@ocaml.doc(" Returns the index of the first occurrence of [string] or None if string has no occurences of [string]

    {2 Examples}

    {[
      String.indexOf \"Hello World World\" \"World\" = Some 6
      String.indexOf \"Hello World World\" \"Bye\" = None 
    ]}
")
let indexOf: (string, string) => option<int>

let index_of: (string, string) => option<int>

@ocaml.doc(" Returns the index of the last occurrence of [string] or None if string has no occurences of [string]

    {2 Examples}

    {[
      String.indexOfRight \"Hello World World\" \"World\" = Some 12
      String.indexOfRight \"Hello World World\" \"Bye\" = None 
    ]}
")
let indexOfRight: (string, string) => option<int>

let index_of_right: (string, string) => option<int>

@ocaml.doc(" Insert a string at [index].

    The character previously at index will now follow the inserted string.

    {2 Examples}

    {[String.insertAt ~insert:\"**\" ~index:2 \"abcde\" = \"ab**cde\"]}

    {[String.insertAt ~insert:\"**\" ~index:0 \"abcde\" = \"**abcde\"]}

    {[String.insertAt ~insert:\"**\" ~index:5 \"abcde\" = \"abcde**\"]}

    {[String.insertAt ~insert:\"**\" ~index:(-2) \"abcde\" = \"abc**de\"]}

    {[String.insertAt ~insert:\"**\" ~index:(-9) \"abcde\" = \"**abcde\"]}

    {[String.insertAt ~insert:\"**\" ~index:9 \"abcde\" = \"abcde**\"]}
")
let insertAt: (string, ~index: int, ~value: t) => string

let insert_at: (string, ~index: int, ~value: t) => string

@ocaml.doc(" Converts all upper case letters to lower case.

    {b Note} This function works only with ASCII characters, not Unicode.

    {2 Examples}

    {[String.toLowercase \"AaBbCc123\" = \"aabbcc123\"]}
")
let toLowercase: string => string

let to_lowercase: string => string

@ocaml.doc(" Converts all lower case letters to upper case.

    {b Note} This function works only with ASCII characters, not Unicode.

    {2 Examples}

    {[String.toUppercase \"AaBbCc123\" = \"AABBCC123\"]}
")
let toUppercase: string => string

let to_uppercase: string => string

@ocaml.doc(" Converts the first letter to lower case if it is upper case.

    {b Note} This function works only with ASCII characters, not Unicode.

    {2 Examples}

    {[String.uncapitalize \"Anastasia\" = \"anastasia\"]}
")
let uncapitalize: string => string

@ocaml.doc(" Converts the first letter of [s] to lowercase if it is upper case.

    {b Note} This function works only with ASCII characters, not Unicode.

    {2 Examples}

    {[String.uncapitalize \"den\" = \"Den\"]}
")
let capitalize: string => string

@ocaml.doc(" Removes leading and trailing {{!Char.isWhitespace} whitespace} from a string

    {2 Examples}

    {[String.trim \"  abc  \" = \"abc\"]}

    {[String.trim \"  abc def  \" = \"abc def\"]}

    {[String.trim \"\r\n\t abc \n\n\" = \"abc\"]}
")
let trim: string => string

@ocaml.doc(" Like {!trim} but only drops characters from the beginning of the string. ")
let trimLeft: string => string

let trim_left: string => string

@ocaml.doc(" Like {!trim} but only drops characters from the end of the string. ")
let trim_right: string => string

@ocaml.doc(" Pad a string up to a minimum length

    If the string is shorted than the proivded length, adds [with] to the left of the string until the minimum length is met

    {2 Examples}

    {[String.padLeft \"5\" 3 ~with_:\"0\" = \"005\"]}
")
let padLeft: (string, int, ~with_: string) => string

let pad_left: (string, int, ~with_: string) => string

@ocaml.doc(" Pad a string up to a minimum length

    If the string is shorted than the proivded length, adds [with] to the left of the string until the minimum length is met

    {2 Examples}

    {[String.padRight \"Ahh\" 7 ~with_:\"h\" = \"Ahhhhhh\"]}
")
let padRight: (string, int, ~with_: string) => string

let pad_right: (string, int, ~with_: string) => string

@@ocaml.text(" {1 Deconstruct} ")

@ocaml.doc(" Returns, as an {!Option}, a tuple containing the first {!Char} and the remaining String.

    If given an empty string, returns [None].

    {2 Examples}

    {[String.uncons \"abcde\" = Some ('a', \"bcde\")]}

    {[String.uncons \"a\" = Some ('a', \"\")]}

    {[String.uncons \"\" = None]}
")
let uncons: string => option<(char, string)>

@ocaml.doc(" Divide a string into a list of strings, splitting whenever [on] is encountered.

    {2 Examples}

    {[
      String.split ~on:\"/\" \"a/b/c\" = [\"a\"; \"b\"; \"c\"]
      String.split ~on:\"--\" \"a--b--c\" = [\"a\"; \"b\"; \"c\"]
      String.split ~on:\"/\" \"abc\" = [\"abc\"]
      String.split ~on:\"/\" \"\" = [\"\"]
      String.split ~on:\"\" \"abc\" = [\"a\"; \"b\"; \"c\"]
    ]}
")
let split: (string, ~on: string) => list<string>

@@ocaml.text(" {1 Iterate} ")

@ocaml.doc(" Run [f] on each character in a string. ")
let forEach: (string, ~f: char => unit) => unit

let for_each: (string, ~f: char => unit) => unit

@ocaml.doc(" Like {!Array.fold} but the elements are {!Char}s  ")
let fold: (string, ~initial: 'a, ~f: ('a, char) => 'a) => 'a

@@ocaml.text(" {1 Convert} ")

@ocaml.doc(" Returns an {!Array} of the individual characters in the given string.

    {2 Examples}

    {[String.toArray \"\" = [||]]}

    {[String.toArray \"abc\" = [|'a'; 'b'; 'c'|]]}
")
let toArray: string => array<char>

let to_array: string => array<char>

@ocaml.doc(" Returns a {!List} of the individual characters in the given string.

    {2 Examples}

    {[String.toList \"\" = []]}

    {[String.toList \"abc\" = ['a'; 'b'; 'c']]}
")
let toList: string => list<char>

let to_list: string => list<char>

@@ocaml.text(" {1 Compare} ")

@ocaml.doc(" Test two string for equality ")
let equal: (string, string) => bool

@ocaml.doc(" Compare two strings. Strings use 'dictionary' ordering.
1
    Also known as {{: https://en.wikipedia.org/wiki/Lexicographical_order } lexicographical ordering }.

    {2 Examples}

    {[String.compare \"Z\" \"A\" = 1]}

    {[String.compare \"Be\" \"Bee\" = -1]}

    {[String.compare \"Pear\" \"pear\" = 1]}

    {[String.compare \"Peach\" \"Peach\" = 0]}
")
let compare: (string, string) => int

@ocaml.doc(" The unique identity for {!Comparator} ")
type identity

let comparator: TableclothComparator.t<t, identity>
