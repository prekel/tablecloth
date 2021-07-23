open Tablecloth
open AlcoJest

let suite = suite("Map - OCaml Syntax", () => {
  let animals = Map.String.fromList(list{("Bears", 2)})
  test(".?{}", () =>
    expect(Map.\".?{}"(animals, "Bears")) |> toEqual(
      {
        open Eq
        option(int)
      },
      Some(2),
    )
  )

  test(".?{}<-", () => {
    open Map
    let withWolves = \".?{}<-"(animals, "Wolves", 15)
    expect(Map.toList(withWolves)) |> toEqual(
      {
        open Eq
        list(pair(string, int))
      },
      list{("Bears", 2), ("Wolves", 15)},
    )
  })
})
