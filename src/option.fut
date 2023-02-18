type option 'a = #some a | #none

def all_somes 'a (xs: [](option a)) (ne: a): []a =
  let is_some (x: option a): bool =
    match x
    case #some _ -> true
    case #none -> false
  let remove_some (x: option a): a =
    match x
    case #some a -> a
    case #none -> assert false ne
  in map remove_some (filter is_some xs)
