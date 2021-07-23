open Tablecloth
open AlcoJest

let suite = suite("List", () => {
  open List

  describe("filterMap", () =>
    test("keeps elements which return Some", () =>
      expect(List.filterMap(list{-1, 80, 99}, ~f=Char.fromCode)) |> toEqual(
        {
          open Eq
          list(char)
        },
        list{'P', 'c'},
      )
    )
  )

  describe("drop", () => {
    test("from an empty list", () =>
      expect(drop(list{}, ~count=1)) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )

    test("zero elements", () =>
      expect(drop(list{1, 2, 3}, ~count=0)) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 2, 3},
      )
    )

    test("the first element", () =>
      expect(drop(list{1, 2, 3}, ~count=1)) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{2, 3},
      )
    )

    test("all elements", () =>
      expect(drop(list{1, 2, 3}, ~count=3)) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )

    test("greater than the number of elements", () =>
      expect(drop(list{1, 2, 3}, ~count=4)) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
  })

  describe("findIndex", () => {
    test("returns the first (index, element) tuple which f returns true for", () =>
      expect(
        findIndex(~f=(index, number) => index > 2 && Int.isEven(number), list{1, 3, 4, 8}),
      ) |> toEqual(
        {
          open Eq
          option(pair(int, int))
        },
        Some((3, 8)),
      )
    )

    test("returns `None` if `f` returns false for all elements ", () =>
      expect(findIndex(~f=(_, _) => false, list{0, 2, 4, 8})) |> toEqual(
        {
          open Eq
          option(pair(int, int))
        },
        None,
      )
    )

    test("returns `None` for an empty array", () =>
      expect(findIndex(~f=(index, number) => index > 2 && Int.isEven(number), list{})) |> toEqual(
        {
          open Eq
          option(pair(int, int))
        },
        None,
      )
    )
  })

  describe("reverse", () => {
    test("empty list", () =>
      expect(reverse(list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("one element", () =>
      expect(reverse(list{0})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{0},
      )
    )
    test("two elements", () =>
      expect(reverse(list{0, 1})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 0},
      )
    )
  })

  describe("map2", () => {
    test("map2 empty lists", () =>
      expect(map2(~f=\"+", list{}, list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("map2 one element", () =>
      expect(map2(~f=\"+", list{1}, list{1})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{2},
      )
    )
    test("map2 two elements", () =>
      expect(map2(~f=\"+", list{1, 2}, list{1, 2})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{2, 4},
      )
    )
  })

  describe("mapWithIndex", () => {
    test("on an empty list", () =>
      expect(mapWithIndex(~f=(i, _) => i, list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("with a single element", () =>
      expect(mapWithIndex(~f=(i, _) => i, list{'a'})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{0},
      )
    )
    test("with two elements", () =>
      expect(mapWithIndex(~f=(i, _) => i, list{'a', 'b'})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{0, 1},
      )
    )
  })

  describe("sliding", () => {
    test("size 1", () =>
      expect(sliding(list{1, 2, 3, 4, 5}, ~size=1)) |> toEqual(
        {
          open Eq
          list(list(int))
        },
        list{list{1}, list{2}, list{3}, list{4}, list{5}},
      )
    )

    test("size 2", () =>
      expect(sliding(list{1, 2, 3, 4, 5}, ~size=2)) |> toEqual(
        {
          open Eq
          list(list(int))
        },
        list{list{1, 2}, list{2, 3}, list{3, 4}, list{4, 5}},
      )
    )

    test("step 3 ", () =>
      expect(sliding(list{1, 2, 3, 4, 5}, ~size=3)) |> toEqual(
        {
          open Eq
          list(list(int))
        },
        list{list{1, 2, 3}, list{2, 3, 4}, list{3, 4, 5}},
      )
    )

    test("size 2, step 2", () =>
      expect(sliding(list{1, 2, 3, 4, 5}, ~size=2, ~step=2)) |> toEqual(
        {
          open Eq
          list(list(int))
        },
        list{list{1, 2}, list{3, 4}},
      )
    )

    test("size 1, step 3", () =>
      expect(sliding(list{1, 2, 3, 4, 5}, ~size=1, ~step=3)) |> toEqual(
        {
          open Eq
          list(list(int))
        },
        list{list{1}, list{4}},
      )
    )

    test("size 2, step 3", () =>
      expect(sliding(list{1, 2, 3, 4, 5}, ~size=2, ~step=3)) |> toEqual(
        {
          open Eq
          list(list(int))
        },
        list{list{1, 2}, list{4, 5}},
      )
    )

    test("step 7", () =>
      expect(sliding(list{1, 2, 3, 4, 5}, ~size=7)) |> toEqual(
        {
          open Eq
          list(list(int))
        },
        list{},
      )
    )
  })

  describe("partition", () => {
    test("empty list", () =>
      expect(partition(~f=Int.isEven, list{})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{}, list{}),
      )
    )
    test("one element", () =>
      expect(partition(~f=Int.isEven, list{1})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{}, list{1}),
      )
    )
    test("four elements", () =>
      expect(partition(~f=Int.isEven, list{1, 2, 3, 4})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{2, 4}, list{1, 3}),
      )
    )
  })

  describe("minimum", () => {
    test("minimum non-empty list", () =>
      expect(minimum(list{7, 9, 15, 10, 3}, ~compare=Int.compare)) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(3),
      )
    )
    test("minimum empty list", () =>
      expect(minimum(list{}, ~compare=Int.compare)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })

  describe("maximum", () => {
    test("maximum non-empty list", () =>
      expect(maximum(list{7, 9, 15, 10, 3}, ~compare=Int.compare)) |> toEqual(
        {
          open Eq
          option(int)
        },
        Some(15),
      )
    )
    test("maximum empty list", () =>
      expect(maximum(list{}, ~compare=Int.compare)) |> toEqual(
        {
          open Eq
          option(int)
        },
        None,
      )
    )
  })

  describe("splitAt", () => {
    test("empty list", () =>
      expect(splitAt(list{}, ~index=1)) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{}, list{}),
      )
    )
    test("at evens", () =>
      expect(splitAt(~index=0, list{2, 4, 6})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{}, list{2, 4, 6}),
      )
    )
    test("four elements", () =>
      expect(splitAt(~index=2, list{1, 3, 2, 4})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{1, 3}, list{2, 4}),
      )
    )
    test("at end", () =>
      expect(splitAt(~index=3, list{1, 3, 5})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{1, 3, 5}, list{}),
      )
    )

    test("past end", () =>
      expect(splitAt(~index=6, list{1, 3, 5})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{1, 3, 5}, list{}),
      )
    )
  })

  describe("splitWhen", () => {
    test("empty list", () =>
      expect(splitWhen(~f=Int.isEven, list{})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{}, list{}),
      )
    )
    test("the first element satisfies f", () =>
      expect(splitWhen(~f=Int.isEven, list{2, 4, 6})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{}, list{2, 4, 6}),
      )
    )
    test("the last element satisfies f", () =>
      expect(splitWhen(~f=Int.isEven, list{1, 3, 2, 4})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{1, 3}, list{2, 4}),
      )
    )
    test("no element satisfies f", () =>
      expect(splitWhen(~f=Int.isEven, list{1, 3, 5})) |> toEqual(
        {
          open Eq
          pair(list(int), list(int))
        },
        (list{1, 3, 5}, list{}),
      )
    )
  })

  describe("intersperse", () => {
    test("intersperse empty list", () =>
      expect(intersperse(list{}, ~sep="on")) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{},
      )
    )
    test("intersperse one turtle", () =>
      expect(intersperse(~sep="on", list{"turtles"})) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{"turtles"},
      )
    )
    test("intersperse three turtles", () =>
      expect(intersperse(~sep="on", list{"turtles", "turtles", "turtles"})) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{"turtles", "on", "turtles", "on", "turtles"},
      )
    )
  })

  describe("initial", () => {
    test("empty list", () =>
      expect(initial(list{})) |> toEqual(
        {
          open Eq
          option(list(int))
        },
        None,
      )
    )
    test("one element", () =>
      expect(initial(list{'a'})) |> toEqual(
        {
          open Eq
          option(list(char))
        },
        Some(list{}),
      )
    )
    test("two elements", () =>
      expect(initial(list{'a', 'b'})) |> toEqual(
        {
          open Eq
          option(list(char))
        },
        Some(list{'a'}),
      )
    )
  })

  describe("append", () => {
    test("append empty lists", () =>
      expect(append(list{}, list{})) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{},
      )
    )
    test("append empty list", () =>
      expect(append(list{}, list{"turtles"})) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{"turtles"},
      )
    )
    test("append empty list", () =>
      expect(append(list{"turtles"}, list{})) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{"turtles"},
      )
    )
    test("append two lists", () =>
      expect(append(list{"on"}, list{"turtles"})) |> toEqual(
        {
          open Eq
          list(string)
        },
        list{"on", "turtles"},
      )
    )
  })

  describe("folds", () => {
    test("empty list", () =>
      expect(fold(~f=cons, ~initial=list{}, list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("one element", () =>
      expect(fold(~f=cons, ~initial=list{}, list{1})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1},
      )
    )
    test("three elements", () =>
      expect(fold(~f=cons, ~initial=list{}, list{1, 2, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{3, 2, 1},
      )
    )
    test("foldr empty list", () =>
      expect(foldRight(~f=cons, ~initial=list{}, list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("foldr one element", () =>
      expect(foldRight(~f=cons, ~initial=list{}, list{1})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1},
      )
    )
    test("foldr three elements", () =>
      expect(foldRight(~f=cons, ~initial=list{}, list{1, 2, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 2, 3},
      )
    )
    test("-", () => expect(fold(~f=\"-", ~initial=0, list{1, 2, 3})) |> toEqual(Eq.int, -6))
    test("- foldRight", () =>
      expect(foldRight(~f=\"-", ~initial=0, list{1, 2, 3})) |> toEqual(Eq.int, -6)
    )
  })

  describe("insertAt", () => {
    test("empty list", () =>
      expect(insertAt(~index=0, ~value=1, list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1},
      )
    )
    test("in the middle", () =>
      expect(insertAt(~index=1, ~value=2, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 2, 3},
      )
    )
    test("in the front", () =>
      expect(insertAt(~index=0, ~value=2, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{2, 1, 3},
      )
    )

    test("after end of list", () =>
      expect(insertAt(~index=4, ~value=2, list{1, 3}) |> toArray) |> toEqual(
        {
          open Eq
          array(int)
        },
        [1, 3, 2],
      )
    )
  })

  describe("updateAt", () => {
    test("updateAt index smaller 0", () =>
      expect(updateAt(~index=-1, ~f=x => x + 1, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 3},
      )
    )
    test("updateAt empty list", () =>
      expect(updateAt(~index=0, ~f=x => x + 1, list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("updateAt empty list", () =>
      expect(updateAt(~index=2, ~f=x => x + 1, list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("updateAt inside the list", () =>
      expect(updateAt(~index=1, ~f=x => x + 1, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 4},
      )
    )
    test("updateAt in the front", () =>
      expect(updateAt(~index=0, ~f=x => x + 1, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{2, 3},
      )
    )
    test("updateAt after end of list", () =>
      expect(updateAt(~index=4, ~f=x => x + 1, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 3},
      )
    )
  })

  describe("flatten", () => {
    test("two empty lists", () =>
      expect(flatten(list{list{}, list{}})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("one empty list", () =>
      expect(flatten(list{list{1}, list{}})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1},
      )
    )
    test("one empty list", () =>
      expect(flatten(list{list{}, list{1}})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1},
      )
    )
    test("several lists", () =>
      expect(flatten(list{list{1}, list{2}, list{3}})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 2, 3},
      )
    )
    test("several lists", () =>
      expect(flatten(list{list{1}, list{}, list{2}, list{}, list{3}})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 2, 3},
      )
    )
  })

  describe("initialize", () => {
    test("initialize length 0", () =>
      expect(initialize(0, ~f=i => i)) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("initialize length 1", () =>
      expect(initialize(1, ~f=i => i)) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{0},
      )
    )
    test("initialize length 2", () =>
      expect(initialize(2, ~f=i => i)) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{0, 1},
      )
    )
  })

  describe("removeAt", () => {
    test("removeAt index smaller 0", () =>
      expect(removeAt(~index=-1, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 3},
      )
    )
    test("removeAt empty list", () =>
      expect(removeAt(~index=0, list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("removeAt empty list", () =>
      expect(removeAt(~index=2, list{})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{},
      )
    )
    test("removeAt index 1", () =>
      expect(removeAt(~index=1, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1},
      )
    )
    test("removeAt index 0", () =>
      expect(removeAt(~index=0, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{3},
      )
    )
    test("removeAt after end of list", () =>
      expect(removeAt(~index=4, list{1, 3})) |> toEqual(
        {
          open Eq
          list(int)
        },
        list{1, 3},
      )
    )
  })

  describe("groupBy", () => {
    test("returns an empty map for an empty array", () =>
      expect(List.groupBy(list{}, module(Int), ~f=String.length) |> Map.length) |> toEqual(
        Eq.int,
        0,
      )
    )

    test("example test case", () => {
      let animals = list{"Ant", "Bear", "Cat", "Dewgong"}
      expect(List.groupBy(animals, module(Int), ~f=String.length) |> Map.toList) |> toEqual(
        {
          open Eq
          list(pair(int, list(string)))
        },
        list{(3, list{"Cat", "Ant"}), (4, list{"Bear"}), (7, list{"Dewgong"})},
      )
    })
  })
})
