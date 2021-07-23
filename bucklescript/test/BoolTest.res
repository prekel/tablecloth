open Tablecloth
open AlcoJest

let suite = suite("Bool", () => {
  open Bool
  describe("fromInt", () => {
    test("converts zero to Some(false)", () =>
      expect(fromInt(0)) |> toEqual(
        {
          open Eq
          option(bool)
        },
        Some(false),
      )
    )

    test("converts one to Some(true)", () =>
      expect(fromInt(1)) |> toEqual(
        {
          open Eq
          option(bool)
        },
        Some(true),
      )
    )

    testAll(
      "converts everything else to None",
      list{Int.minimumValue, -2, -1, 2, Int.maximumValue},
      int =>
        expect(fromInt(int)) |> toEqual(
          {
            open Eq
            option(bool)
          },
          None,
        ),
    )
  })
})
