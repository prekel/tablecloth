open Tablecloth
open AlcoJest

let suite = suite("String - OCaml Syntax", () => {
  let animal = "Salmon"
  describe(".[]", () =>
    test("regular string syntax is the equivalent to String.get", () =>
      expect(String.get(animal, 0)) |> toEqual(Eq.char, 'S')
    )
  )
  describe(".?[]", () => {
    open String
    test("in bounds index returns Some", () =>
      expect(\".?[]"(animal, 1)) |> toEqual(
        {
          open Eq
          option(char)
        },
        Some('a'),
      )
    )
    test("out of bounds index returns None", () =>
      expect(\".?[]"(animal, 9)) |> toEqual(
        {
          open Eq
          option(char)
        },
        None,
      )
    )
  })
})
