open Tablecloth
open AlcoJest

let suite = suite("Array - OCaml Syntax", () => {
  let animals = ["Bear", "Wolf"]
  describe(".()", () =>
    test("regular array syntax is the equivalent to Array.get", () =>
      expect(animals[0]) |> toEqual(Eq.string, "Bear")
    )
  )
  describe(".?()", () => {
    open Array
    test("in bounds index returns Some", () =>
      expect(\".?()"(animals, 1)) |> toEqual(
        {
          open Eq
          option(string)
        },
        Some("Wolf"),
      )
    )
    test("out of bounds index returns None", () =>
      expect(\".?()"(animals, 3)) |> toEqual(
        {
          open Eq
          option(string)
        },
        None,
      )
    )
  })
})
