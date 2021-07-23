/* This file is used to target different OCaml versions using the BuckleScript
   preprocessor. This is a separate file since ocamlformat does not accept
   the BuckleScript preprocessor. */

module String = {
  let toLower = (s: string): string => String.lowercase_ascii(s)

  let toUpper = (s: string): string => String.uppercase_ascii(s)

  let uncapitalize = (s: string): string => String.uncapitalize_ascii(s)

  let capitalize = (s: string): string => String.capitalize_ascii(s)

  let isCapitalized = (s: string): bool => s == String.capitalize_ascii(s)
}
