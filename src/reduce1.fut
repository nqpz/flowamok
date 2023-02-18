-- Taken from https://futhark-lang.org/examples/no-neutral-element.html
type with_neutral 't = #neutral | #val t

def f_with_neutral 't (f: t -> t -> t)
                      (x: with_neutral t)
                      (y: with_neutral t)
                      : with_neutral t =
  match (x, y)
  case (#val x, #val y) -> #val (f x y)
  case (#neutral, _) -> y
  case (_, #neutral) -> x

def reduce1 't (f: t -> t -> t) (ts: []t) : with_neutral t =
  reduce (f_with_neutral f) #neutral (map (\t -> #val t) ts)
