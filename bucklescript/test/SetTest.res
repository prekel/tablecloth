open Tablecloth
open AlcoJest

module Coordinate = {
  type t = (int, int)
  let compare = Tuple2.compare(Int.compare, Int.compare)
  include Comparator.Make({
    type t = t
    let compare = compare
  })
}

let coordinate = {
  let eq = (a: Coordinate.t, b: Coordinate.t): bool => a == b
  let pp = (formatter, (x, y)) =>
    Format.pp_print_string(
      formatter,
      "(" ++ (Int.toString(x) ++ (", " ++ (Int.toString(y) ++ ")"))),
    )

  Eq.make(eq, pp)
}

let suite = suite("Set", () => {
  test("creates a set from a list", () => {
    let set = Set.fromList(module(Int), list{1, 2})
    expect(Set.includes(set, 1)) |> toEqual(Eq.bool, true)
  })

  test("fromArray", () => {
    let set = Set.fromArray(module(Coordinate), [(0, 0), (0, 1)])
    expect(Set.includes(set, (0, 1))) |> toEqual(Eq.bool, true)
  })

  test("union", () => {
    let xAxis = Set.fromList(module(Coordinate), list{(0, 0), (0, 1)})
    let yAxis = Set.fromList(module(Coordinate), list{(0, 0), (1, 0)})
    let union = Set.union(xAxis, yAxis)
    expect(union |> Set.toArray) |> toEqual(Eq.array(coordinate), [(0, 0), (0, 1), (1, 0)])
  })

  describe("Int", () =>
    test("creates a set from a list", () => {
      let set = Set.Int.fromList(list{1, 2})
      expect(Set.includes(set, 1)) |> toEqual(Eq.bool, true)
    })
  )

  describe("String", () =>
    test("creates a set from a list", () => {
      let set = Set.String.fromList(list{"Ant", "Bat"})
      expect(Set.includes(set, "Ant")) |> toEqual(Eq.bool, true)
    })
  )
})
