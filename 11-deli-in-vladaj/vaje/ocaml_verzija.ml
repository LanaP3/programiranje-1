let rec merge target list_1 list_2 =
  match list_1 list_2 with
  | [],[] -> target
  | [],list_2 -> target @ list_2
  | list_1,[] -> list_1 @ target
  | x::xs, y::ys when x<y -> merge (x::target) xs (y::ys)
  | x::xs, y::ys -> merge (y::target) (x::xs) ys