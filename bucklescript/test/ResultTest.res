open Tablecloth
open AlcoJest

let suite = suite("Result", () => {
  open Result
  describe("fromOption", () => {
    test("maps None into Error", () =>
      expect(fromOption(~error="error message", None)) |> toEqual(
        {
          open Eq
          result(int, string)
        },
        Error("error message"),
      )
    )

    test("maps Some into Ok", () =>
      expect(fromOption(~error="error message", Some(10))) |> toEqual(
        {
          open Eq
          result(int, string)
        },
        Ok(10),
      )
    )
  })
})
