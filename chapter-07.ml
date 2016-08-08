(* 1 *)
let identity x = x

(* 5 *)
let true_maker x = true

(* 7 *)
type bool_or_int =
    Hot of bool
  | Cold of int

(* 11 *)
(* Constructors are not functions in OCaml,
 * so the definition for this is a bit different. *)
let hot_maker x =
  fun x -> Hot x

(* 20 *)
let help f =
  Hot (true_maker
    (if true_maker 5
     then f
     else true_maker))

(* 38 *)
type chain =
  Link of int * (int -> chain)

(* 50 *)
let rec ints n =
  Link (n + 1, ints)

(* 58 *)
let rec skips n =
  Link (n + 2, ints)

(* 61 *)
let eq_int ((n : int), (m : int)) = (n = m)

let divides_evenly (n, c) =
  eq_int ((n mod c), 0)

let is_mod_5_or_7 n =
  divides_evenly (n, 5) || divides_evenly (n, 7)

(* 62 *)
let rec some_ints n =
  if is_mod_5_or_7 (n + 1)
  then Link (n + 1, some_ints)
  else some_ints (n + 1)

(* 83 *)
let rec chain_item =
  fun (n, Link (i, f)) ->
    if eq_int (n, 1)
    then i
    else chain_item (n - 1, f i)

(* 93 *)
let rec is_prime n =
  has_no_divisors (n, n - 1)
and has_no_divisors (n, c) =
  eq_int (c, 1) ||
    if divides_evenly (n, c)
    then false
    else has_no_divisors (n, c - 1)
(* Nicer: *)
let is_prime n =
  let rec has_no_divisors (n, c) =
    eq_int (c, 1) ||
      if divides_evenly (n, c)
      then false
      else has_no_divisors (n, c - 1)
  in has_no_divisors (n, n - 1)

(* 96 *)
let rec primes n =
  if is_prime (n + 1)
  then Link (n + 1, primes)
  else primes (n + 1)

(* 98 *)
let rec fibs n m =
  Link (n + m, fibs m)

(* 109 *)
let fibs_1 m =
  Link (1 + m, fibs m)

(* 117 *)
let fibs_2 m =
  Link (2 + m, fibs m)
