(* 6 *)
type fruit = Peach | Apple | Pear | Lemon | Fig

type tree =
    Bud
  | Flat of fruit * tree
  | Split of tree * tree

(* 11 *)
let rec flat_only =
  function
    Bud -> true
  | Flat (f, t) -> flat_only t
  | Split (s, t) -> false

(* 18 *)
let rec split_only =
  function
    Bud -> true
  | Flat (f, t) -> false
  | Split (s, t) ->
      split_only s && split_only t

(* 22 *)
let rec contains_fruit =
  function
    Bud -> false
  | Flat (f, t) -> true
  | Split (s, t) ->
      contains_fruit s || contains_fruit t

let contains_fruit x =
  not (split_only x)

(* 34 *)
let less_than ((n : int), (m : int)) =
  (n < m)

let larger_of (n, m) =
  if less_than (n, m)
  then m
  else n

(* 39 *)
let eq_fruit =
  function
    Peach, Peach -> true
  | Apple, Apple -> true
  | Pear, Pear -> true
  | Lemon, Lemon -> true
  | Fig, Fig -> true
  | a_fruit, another_fruit -> false

(* 41 *)
let rec subst_in_tree =
  function
    n, a, Bud -> Bud
  | n, a, Flat (f, t) ->
      if eq_fruit (f, a)
      then Flat (n, subst_in_tree (n, a, t))
      else Flat (f, subst_in_tree (n, a, t))
  | n, a, Split (s, t) ->
      Split (subst_in_tree (n, a, s),
             subst_in_tree (n, a, t))

(* 43 *)
let rec occurs =
  function
    n, Bud -> 0
  | n, Flat (f, t) ->
      if eq_fruit (n, f)
      then 1 + occurs (n, t)
      else occurs (n, t)
  | n, Split (s, t) ->
      occurs (n, s) + occurs (n, t)

(* 51 *)
type 'a slist =
    Empty
  | Scons of 'a sexp * 'a slist
and 'a sexp =
    An_atom of 'a
  | A_slist of 'a slist

(* 57 *)
let rec occurs_in_slist =
  function
    a, Empty -> 0
  | a, Scons (s, y) ->
      occurs_in_sexp (a, s) + occurs_in_slist (a, y)
and occurs_in_sexp =
  function
    a, An_atom b ->
      if eq_fruit (b, a) then 1 else 0
  | a, A_slist y -> occurs_in_slist (a, y)

(* 58 *)
let rec subst_in_slist =
  function
    n, a, Empty -> Empty
  | n, a, Scons (s, y) ->
      Scons (subst_in_sexp (n, a, s),
             subst_in_slist (n, a, y))
and subst_in_sexp =
  function
    n, a, An_atom b ->
      if eq_fruit (b, a)
      then An_atom n
      else An_atom b
  | n, a, A_slist y ->
      A_slist (subst_in_slist (n, a, y))

(* 65 *)
let rec eq_fruit_in_atom =
  function
    a, An_atom s -> eq_fruit (a, s)
  | a_fruit, A_slist y -> false

(* 68 *)
let rec rem_from_slist =
  function
    a, Empty -> Empty
  | a, Scons (s, y) ->
      if eq_fruit_in_atom (a, s)
      then rem_from_slist (a, y)
      else Scons (rem_from_sexp (a, s),
                  rem_from_slist (a, y))
and rem_from_sexp =
  function
    a, An_atom b -> An_atom b
  | a, A_slist y -> A_slist (rem_from_slist (a, y))

(* 76 *)
let rec rem_from_slist =
  function
    a, Empty -> Empty
  | a, Scons (An_atom b, y) ->
      if eq_fruit (a, b)
      then rem_from_slist (a, y)
      else Scons (An_atom b,
                  rem_from_slist (a, y))
  | a, Scons (A_slist x, y) ->
      Scons (A_slist (rem_from_slist (a, x)),
             rem_from_slist (a, y))
