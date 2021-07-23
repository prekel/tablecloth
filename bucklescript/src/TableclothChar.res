/*
  This file has a `Tablecloth` prefix since it uses Stdlib.Char in its implementation.
  Without the prefix we would encounter circular reference compiler errors.
*/

type t = char

include TableclothComparator.Make({
  type t = t

  let compare = compare
})

let toCode = (c: char) => Char.code(c)

let to_code = toCode

let fromCode = (i): option<char> =>
  if 0 <= i && i <= 255 {
    Some(Char.chr(i))
  } else {
    None
  }

let from_code = fromCode

let toString = c => String.make(1, c)

let to_string = toString

let fromString = (str): option<char> =>
  switch String.length(str) {
  | 1 => Some(String.get(str, 0))
  | _ => None
  }

let from_string = fromString

let toDigit = char =>
  switch char {
  | '0' .. '9' => Some(toCode(char) - toCode('0'))
  | _ => None
  }

let to_digit = toDigit

let toLowercase = char =>
  switch char {
  | 'A' .. 'Z' => Char.chr(toCode('a') + (toCode(char) - toCode('A')))
  | _ => char
  }

let to_lowercase = toLowercase

let toUppercase = char =>
  switch char {
  | 'a' .. 'z' => Char.chr(toCode('A') + (toCode(char) - toCode('a')))
  | _ => char
  }

let to_uppercase = toUppercase

let isLowercase = x =>
  switch x {
  | 'a' .. 'z' => true
  | _ => false
  }

let is_lowercase = isLowercase

let isUppercase = x =>
  switch x {
  | 'A' .. 'Z' => true
  | _ => false
  }

let is_uppercase = isUppercase

let isLetter = x =>
  switch x {
  | 'a' .. 'z' | 'A' .. 'Z' => true
  | _ => false
  }

let is_letter = isLetter

let isDigit = x =>
  switch x {
  | '0' .. '9' => true
  | _ => false
  }

let is_digit = isDigit

let isAlphanumeric = x =>
  switch x {
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' => true
  | _ => false
  }

let is_alphanumeric = isAlphanumeric

let isPrintable = x =>
  switch x {
  | ' ' .. '~' => true
  | _ => false
  }

let is_printable = isPrintable

let isWhitespace = x =>
  switch x {
  | '\t' | '\n' | '\011' | '\012' | '\r' | ' ' => true
  | _ => false
  }

let is_whitespace = isWhitespace

let equal = \"="

let compare = compare
