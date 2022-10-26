let show = (fn, list) => {
  list->Belt.List.reduce("", (a, b) => a ++ fn(b))
}
