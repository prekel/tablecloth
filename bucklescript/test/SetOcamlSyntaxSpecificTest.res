open Tablecloth
open AlcoJest

let suite = suite("Set - OCaml Syntax", () =>
  describe(".?{}", () => {
    let animals = Set.String.fromList(list{"Bear", "Wolf"})
    test("custom index operators can be used in the ocaml syntax", () => {
      open Set
      expect(\".?{}"(animals, "Bear")) |> toEqual(
        {
          open Eq
          bool
        },
        true,
      )
    })
  })
)
