(* 1 *)
type 'a pizza =
    Bottom
  | Topping of ('a * 'a pizza)

(* 3 *)
type fish = Anchovy | Lox | Tuna

(* 11 *)
let rec rem_anchovy =
  function
    Bottom -> Bottom
  | Topping (Anchovy, p) -> rem_anchovy p
  | Topping (Tuna, p) -> Topping (Tuna, rem_anchovy p)
  | Topping (Lox, p) -> Topping (Lox, rem_anchovy p)

(* 13 *)
let rec rem_anchovy =
  function
    Bottom -> Bottom
  | Topping (Anchovy, p) -> rem_anchovy p
  | Topping (t, p) -> Topping (t, rem_anchovy p)

(* 14 *)
let rec rem_tuna =
  function
    Bottom -> Bottom
  | Topping (Anchovy, p) -> Topping (Anchovy, rem_tuna p)
  | Topping (Tuna, p) -> rem_tuna p
  | Topping (Lox, p) -> Topping (Lox, rem_tuna p)

(* 17 *)
type fish = Tuna | Lox | Anchovy

(* 21 *)
let rec rem_tuna =
  function
    Bottom -> Bottom
  | Topping (Tuna, p) -> rem_tuna p
  | Topping (t, p) -> Topping (t, rem_tuna p)

(* 38 *)
let rec eq_fish =
  function
    Anchovy, Anchovy -> true
  | Lox, Lox -> true
  | Tuna, Tuna -> true
  | a_fish, another_fish -> false

(* 40 *)
let rec rem_fish =
  function
    x, Bottom -> Bottom
  | x, Topping (t, p) ->
      if eq_fish (t, x)
      then rem_fish (x, p)
      else Topping (t, (rem_fish (x, p)))

(* 57 *)
let eq_int ((n : int), (m : int)) = (n = m)
(* This also works*)
let eq_int : int * int -> bool =
  fun (n, m) -> (n = m)

let rec rem_int =
  function
    x, Bottom -> Bottom
  | x, Topping (t, p) ->
      if eq_int (t, x)
      then rem_int (x, p)
      else Topping (t, (rem_int (x, p)))

(* 66 *)
let rec subst_fish =
  function
    n, a, Bottom -> Bottom
  | n, a, Topping (t, p) ->
      if eq_fish (t, a)
      then Topping (n, subst_fish (n, a, p))
      else Topping (t, subst_fish (n, a, p))

let rec subst_int =
  function
    n, a, Bottom -> Bottom
  | n, a, Topping (t, p) ->
      if eq_int (t, a)
      then Topping (n, subst_int (n, a, p))
      else Topping (t, subst_int (n, a, p))

(* 71 *)
let rec eq_num =
  function
    Zero, Zero -> true
  | One_more_than n, One_more_than m -> eq_num (n, m)
  | n, m -> false
