type t<'a> = list<'a>

let empty = list{}

let singleton = x => list{x}

let fromArray = array => List.init(Array.length(array), i => array[i])

let from_array = fromArray

let range = (~from=0, to_) => List.init(to_ - from, i => i + from)

let rec repeat = (element, ~times) =>
  if times <= 0 {
    list{}
  } else {
    list{element, ...repeat(element, ~times=times - 1)}
  }

let flatten = Belt.List.flatten

let reverse = Belt.List.reverse

let append = Belt.List.concat

let sum = (type a, t, module(M: TableclothContainer.Sum with type t = a)) =>
  List.fold_left(M.add, M.zero, t)

let map = (t, ~f) => Belt.List.map(t, f)

let flatMap = (t, ~f) => flatten(map(t, ~f))

let flat_map = flatMap

let mapWithIndex = (list, ~f) => Belt.List.mapWithIndex(list, f)

let map_with_index = mapWithIndex

let map2 = (a, b, ~f) => Belt.List.zipBy(a, b, f)

let zip = map2(~f=(a, b) => (a, b))

let rec map3 = (a, b, c, ~f) =>
  switch (a, b, c) {
  | (list{x, ...xs}, list{y, ...ys}, list{z, ...zs}) => list{f(x, y, z), ...map3(xs, ys, zs, ~f)}
  | _ => list{}
  }

let rec last = l =>
  switch l {
  | list{} => None
  | list{x} => Some(x)
  | list{_, ...rest} => last(rest)
  }

let unzip = list => (List.map(((a, _)) => a, list), List.map(((_, b)) => b, list))

let includes = (t, value, ~equal) => Belt.List.has(t, value, equal)

let find = (t, ~f) => Belt.List.getBy(t, f)

let getAt = (t, ~index) => Belt.List.get(t, index)

let get_at = getAt

let any = (t, ~f) => List.exists(f, t)

let head = l => Belt.List.head(l)

let drop = (t, ~count) => Belt.List.drop(t, count)->Belt.Option.getWithDefault(list{})

let take = (t, ~count) => Belt.List.take(t, count)->Belt.Option.getWithDefault(list{})

let initial = l =>
  switch reverse(l) {
  | list{} => None
  | list{_, ...rest} => Some(reverse(rest))
  }

let filterMap = (t, ~f) => Belt.List.keepMap(t, f)

let filter_map = filterMap

let filter = (t, ~f) => Belt.List.keep(t, f)

let filterWithIndex = (t, ~f) => Belt.List.keepWithIndex(t, (e, i) => f(i, e))

let filter_with_index = filterWithIndex

let partition = (t, ~f) => Belt.List.partition(t, f)

let fold = (t, ~initial, ~f) => Belt.List.reduce(t, initial, f)

let count = (t, ~f) => fold(t, ~initial=0, ~f=(total, element) => total + (f(element) ? 1 : 0))

let foldRight = (t, ~initial, ~f) => Belt.List.reduceReverse(t, initial, f)

let fold_right = foldRight

let findIndex = (list, ~f) => {
  let rec loop = (i, l) =>
    switch l {
    | list{} => None
    | list{x, ...rest} =>
      if f(i, x) {
        Some(i, x)
      } else {
        loop(i + 1, rest)
      }
    }

  loop(0, list)
}

let find_index = findIndex

let splitAt = (t, ~index) => {
  if index < 0 {
    raise(Invalid_argument("List.splitAt called with negative index"))
  }
  let rec loop = (front, back, i) =>
    switch back {
    | list{} => (t, list{})
    | list{element, ...rest} =>
      if i == 0 {
        (reverse(front), back)
      } else {
        loop(list{element, ...front}, rest, i - 1)
      }
    }

  loop(list{}, t, index)
}

let split_at = splitAt

let updateAt: (t<'a>, ~index: int, ~f: 'a => 'a) => t<'a> = (t, ~index, ~f) =>
  Belt.List.mapWithIndex(t, (i, element) =>
    if i == index {
      f(element)
    } else {
      element
    }
  )

let update_at = updateAt

let length = l => Belt.List.length(l)

let rec dropWhile = (t, ~f) =>
  switch t {
  | list{} => list{}
  | list{x, ...rest} =>
    if f(x) {
      dropWhile(rest, ~f)
    } else {
      t
    }
  }

let drop_while = dropWhile

let isEmpty = t => t == list{}

let is_empty = isEmpty

let sliding = (~step=1, t, ~size) => {
  let rec loop = t =>
    if isEmpty(t) {
      list{}
    } else {
      let sample = Belt.List.take(t, size)
      let rest = Belt.List.drop(t, step)
      switch (sample, rest) {
      | (None, _) => list{}
      | (Some(x), None) => list{x}
      | (Some(x), Some(xs)) => list{x, ...loop(xs)}
      }
    }

  loop(t)
}

let chunksOf = (t, ~size) => sliding(t, ~step=size, ~size)

let chunks_of = chunksOf

let cons = (t, element) => list{element, ...t}

let takeWhile = (t, ~f) => {
  let rec takeWhileHelper = (acc, t) =>
    switch t {
    | list{} => reverse(acc)
    | list{x, ...rest} =>
      if f(x) {
        takeWhileHelper(list{x, ...acc}, rest)
      } else {
        reverse(acc)
      }
    }

  takeWhileHelper(list{}, t)
}

let take_while = takeWhile

let all = (t, ~f) => Belt.List.every(t, f)

let tail = t =>
  switch t {
  | list{} => None
  | list{_, ...rest} => Some(rest)
  }

let removeAt = (t, ~index) =>
  if index < 0 {
    t
  } else {
    let (front, back): (t<'a>, t<'a>) = splitAt(t, ~index)
    switch tail(back) {
    | None => t
    | Some(t) => append(front, t)
    }
  }

let remove_at = removeAt

let minimum = (t, ~compare) =>
  fold(t, ~initial=None, ~f=(min, element) =>
    switch min {
    | None => Some(element)
    | Some(value) => compare(element, value) < 0 ? Some(element) : min
    }
  )

let maximum = (t, ~compare) =>
  fold(t, ~initial=None, ~f=(max, element) =>
    switch max {
    | None => Some(element)
    | Some(value) => compare(element, value) > 0 ? Some(element) : max
    }
  )

let extent = (t, ~compare) =>
  fold(t, ~initial=None, ~f=(current, element) =>
    switch current {
    | None => Some(element, element)
    | Some(min, max) =>
      Some(compare(element, min) < 0 ? element : min, compare(element, max) > 0 ? element : max)
    }
  )

let sort = (t, ~compare) => Belt.List.sort(t, compare)

let span = (t, ~f) =>
  switch t {
  | list{} => (list{}, list{})
  | _ => (takeWhile(t, ~f), dropWhile(t, ~f))
  }

let rec groupWhile = (t, ~f) =>
  switch t {
  | list{} => list{}
  | list{x, ...rest} =>
    let (ys, zs) = span(rest, ~f=f(x))
    list{list{x, ...ys}, ...groupWhile(zs, ~f)}
  }

let group_while = groupWhile

let insertAt = (t, ~index, ~value) => {
  if index < 0 {
    raise(Invalid_argument("List.splitAt called with negative index"))
  }
  let rec loop = (front, back, i) =>
    switch back {
    | list{} => reverse(list{value, ...front})
    | list{element, ...rest} =>
      if i == 0 {
        append(reverse(front), list{value, element, ...rest})
      } else {
        loop(list{element, ...front}, rest, index - 1)
      }
    }

  loop(list{}, t, index)
}

let insert_at = insertAt

let splitWhen = (t, ~f) => {
  let rec loop = (front, back) =>
    switch back {
    | list{} => (t, list{})
    | list{element, ...rest} =>
      if f(element) {
        (reverse(front), back)
      } else {
        loop(list{element, ...front}, rest)
      }
    }

  loop(list{}, t)
}

let split_when = splitWhen

let intersperse = (t, ~sep) =>
  switch t {
  | list{} => list{}
  | list{x} => list{x}
  | list{x, ...rest} => list{
      x,
      ...foldRight(rest, ~initial=list{}, ~f=(acc, x) => list{sep, x, ...acc}),
    }
  }

let initialize = (length, ~f) => Belt.List.makeBy(length, f)

let forEach = (t, ~f): unit => Belt.List.forEach(t, f)

let for_each = forEach

let forEachWithIndex = (t, ~f): unit => Belt.List.forEachWithIndex(t, f)

let for_each_with_index = forEachWithIndex

let toArray = Array.of_list

let to_array = toArray

let join = (strings, ~sep) => Js.Array.joinWith(sep, toArray(strings))

let groupBy = (t, comparator, ~f) =>
  fold(t, ~initial=TableclothMap.empty(comparator), ~f=(map, element) => {
    let key = f(element)
    TableclothMap.update(map, ~key, ~f=x =>
      switch x {
      | None => Some(list{element})
      | Some(elements) => Some(list{element, ...elements})
      }
    )
  })

let group_by = groupBy

let rec equal = (equalElement, a, b) =>
  switch (a, b) {
  | (list{}, list{}) => true
  | (list{x, ...xs}, list{y, ...ys}) => equalElement(x, y) && equal(equalElement, xs, ys)
  | _ => false
  }

let rec compare = (compareElement, a, b) =>
  switch (a, b) {
  | (list{}, list{}) => 0
  | (list{}, _) => -1
  | (_, list{}) => 1
  | (list{x, ...xs}, list{y, ...ys}) =>
    switch compareElement(x, y) {
    | 0 => compare(compareElement, xs, ys)
    | result => result
    }
  }
